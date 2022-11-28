--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE TypeOperators #-}

module Main (initRoadSample, main, trainModel) where

import Debug.Trace ( trace )
import System.Environment ( getArgs )
import Data.Kind (Constraint)
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import System.Random (randomIO)
import CppFFI (Scene (..), Camera (..), Texture,
               getSceneFBO, renderScene, findTextureDifference,
               getMeanPixelValue, testBed, setTargetImg, createTextureFBO,
               TextureFBO (frameBuffer, texture), getTargetTexture, getHoughLines, screenBuffer)
import Data.List (partition)
import Foreign.C.String
import Foreign
    ( Storable(..), StablePtr(..), Int32, malloc, Storable(poke) )
import Foreign.Marshal.Alloc
import OpenSum (Member)
import System.IO.Unsafe
import Hough (compareLines)
import Model (Model, uniform, normal)
import Sampler (sampleIO, liftS, sampleIOCustom, Sampler)
import Inference.SIM (simulate)
import Inference.MH (mh)
import Control.Effect.ObsReader (ask)
import Control.Carrier.ObsReader (runObsReader)
import Control.Algebra (run, Has)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Control.Monad (join)

--- Probabilistic Model


--- Training
type RoadEnv =
 '[ "x"         := Double
  , "y"         := Double
  , "z"         := Double
  , "pitch"     := Double
  , "yaw"       := Double
  , "roll"      := Double
  , "error"     := Double
 ]
emptyRoadEnv :: Env RoadEnv
emptyRoadEnv = #x := [] <:> #y := [] <:> #z := [] <:> #pitch := [] <:> #yaw := [] <:> #roll := [] <:> #error := repeat 0 <:> nil

-- TODO: Implement
envToScene :: Env RoadEnv -> Maybe Scene
envToScene env = run $ runObsReader env $ do
    mX <- ask @RoadEnv #x
    let mY = Just 0.2
    -- mY <- ask @RoadEnv #y
    let mZ = Just 0
    -- mZ <- ask @RoadEnv #z
    let mPitch = Just 0
    -- mPitch <- ask @RoadEnv #pitch
    let mYaw = Just 0
    -- mYaw <- ask @RoadEnv #yaw
    let mRoll = Just 0
    -- mRoll <- ask @RoadEnv #roll
    let mError = Just 0
    -- mError <- ask @RoadEnv #error

    return $ case (mX, mY, mZ, mPitch, mYaw, mRoll, mError) of
        (Just x, Just y, Just z, Just pitch, Just yaw, Just roll, Just error) -> Just $ Scene {
            camera = Camera {
                x = x,
                y = y,
                z = z,
                pitch = pitch,
                yaw = yaw,
                roll = roll
            }
        }
        _ -> Nothing

sceneToEnv :: Scene -> Env RoadEnv
sceneToEnv = undefined

sceneToVec :: Scene -> [Double]
sceneToVec scene = zipWith normalise
    [xRange] --, yRange, zRange, pitchRange, yawRange, rollRange]
    (map ($ camera scene) [x]) -- , y, z, pitch, yaw, roll])

xRange, yRange, zRange, pitchRange, yawRange, rollRange :: (Double, Double)
xRange = (-0.4, 0.4)
yRange = (0.05, 2)
zRange = (-1, 1)
pitchRange = (-0.2, 0.2)
yawRange = (0.2, 0.3)
rollRange = (0.2, 0.3)

(...) = (.)(.)(.)

euclidian :: Floating a => [a] -> [a] -> a
euclidian = (sqrt . sum) ... zipWith (join (*) ... subtract)

normalise :: (Double, Double) -> Double -> Double
-- PRE: lower <= value <= upper
-- POST: 0 <= normalise (lower, upper) value <= 1
-- POST: a < b => normalise (l, u) a < normalise (l, u) b
normalise (lower, upper) value = (value - lower) / (upper - lower)

sceneAccuracy :: Scene -> Scene -> Double
-- POST: 0 <= sceneAccuracy s s' <= 1
sceneAccuracy scene scene' = 1 - normalise (0, sqrt n) (euclidian sv (sceneToVec scene'))
    where
        n  = fromIntegral $ length sv
        sv = sceneToVec scene

clamp :: (Double, Double) -> Double -> Double
clamp (a, b) = min b . max a

initRoadSample :: forall env sig m. (Observables env '["x", "y", "z", "pitch", "yaw", "roll"] Double, Has (Model env) sig m) => m Scene
initRoadSample = do
    x <- uniform @env (-0.5) 0.5 #x
    y <- uniform @env (0.05) 0.5 #y
    z <- uniform @env (-0.5) 0.5 #z
    pitch <- uniform @env (-0.2) 0.2 #pitch
    yaw <- uniform @env (-0.2) 0.2 #yaw
    roll <- uniform @env (-0.2) 0.2 #roll

    -- Charlie and Ethan hand tuned ranges
    -- x <- uniform @RoadEnv (fst xRange) (snd xRange) #x
    -- y <- uniform @RoadEnv 0.05 2 #y
    -- z <- uniform @RoadEnv (-1) 1 #z
    -- pitch <- uniform @RoadEnv (-0.2) 0.2 #pitch
    -- yaw <- normal @env 0.2 0.3 #yaw
    -- roll <- normal @env 0.2 0.3 #roll

    return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=z, yaw=0.0, roll=roll} }

roadGenerationModel :: forall env sig m. (Observables env ["x", "y", "z", "pitch", "yaw", "roll", "error"] Double, Has (Model env) sig m) => m ()
roadGenerationModel = do
    roadSample <- initRoadSample @env
    error <- normal @env (errorFunction roadSample) 50 #error
    return ()

--- Main code

--- Do not use this in non-thread-safe code, please 

errorFunction :: Scene -> Double
errorFunction s = unsafePerformIO $ do
    scene <- malloc
    poke scene s

    sceneFBO <- getSceneFBO -- returns global sceneFBO
    
    renderScene scene sceneFBO
    findTextureDifference 0
    first_error <- getMeanPixelValue 0

    findTextureDifference 1
    second_error <- getMeanPixelValue 1

    findTextureDifference 2
    third_error <- getMeanPixelValue 2
    print (first_error + second_error + third_error)

    return (first_error + second_error + third_error)

    findTextureDifference -- implicitly uses global sceneTexture
                          -- and targetTexture
    getMeanPixelValue

testBedExample :: IO Int32
testBedExample = testBed (0.11319984526740867) (0.3784490271439612) (0.0) (-0.1) (0.0) (0.0)

--- Run training loop on the image stored in frameBuffer

trainModelSampler :: Texture -> Sampler Scene
trainModelSampler texture = do
    traces <- mh 100 roadGenerationModel emptyRoadEnv ["x", "y", "z", "pitch", "yaw", "roll"]

    return $ fromJust $ envToScene $ head traces

trainModel :: Texture -> IO Scene
trainModel = sampleIO . trainModelSampler

trainModelSeed :: Int -> Texture -> IO Scene
trainModelSeed seed = sampleIOCustom seed . trainModelSampler

trainModelFromFile :: String -> IO Scene
trainModelFromFile path = do
    setTargetImg path -- reads file in path to targetTexture

    targetTexture <- getTargetTexture
    trainModel targetTexture

benchmark :: Int -> IO Double
benchmark seed = do
    -- Create a scene to try and run the algorithm on.
    (scene, _) <- sampleIOCustom seed $ simulate emptyRoadEnv initRoadSample
    -- let scene = Scene {
    --   camera = Camera { 
    --     x     = -0.4,
    --     y     = 0.2,
    --     z     = 0,
    --     pitch = 0,
    --     yaw   = 0,
    --     roll  = 0
    --   }
    -- }

    -- putStrLn "Input Scene:"
    -- print scene
    -- renderScene scene screenBuffer

    -- Render this scene into an image
    targetTextureFBO <- createTextureFBO
    renderScene scene (frameBuffer targetTextureFBO)

    -- Use mh to work backwards to the origional scene
    putStrLn ("Running algorithm on input seed..." ++ show seed)
    predictedScene <- trainModelSeed seed (texture targetTextureFBO)

    -- putStrLn "Press any key to see predicted scene"
    -- getChar >>= print
    -- putStrLn "Predicted Scene:"
    -- print predictedScene
    -- renderScene predictedScene screenBuffer

    let accuracy = sceneAccuracy scene predictedScene
    putStrLn ("FINAL ACCURACY: " ++ show accuracy)

    -- putStrLn "Press any key to exit"
    -- getChar >>= print
    return accuracy

    -- return (undefined scene predictedScene)

-- Scene {camera = Camera {x = 0.6879765442476931, y = 0.10672289591094897, z = 6.263997681267841e-4, pitch = 0.30004201200001984, yaw = 0.0, roll = 0.0}}
-- Scene {camera = Camera {x = 1.9423491560s450756e-2, y = 0.13973177834102612, z = 6.743796537081953e-3, pitch = -0.20179312329488874, yaw = 0.0, roll = 0.

main :: IO ()
-- main = do
--     scene <- trainModelFromFile "data/synthetic.png"

--     print scene
--     renderScene scene screenBuffer
--     getChar

--     return ()

main = do
    args <- getArgs

    seed <- case map readMaybe args of
        [Just seed] -> return seed
        _ -> do
            seed <- randomIO
            putStrLn ("No seed provided, using seed " ++ show seed)
            return seed

    benchmark seed

    return ()

-- main = dow
--     let imgHls = getHoughLines "data/road.jpg"
--     let err = compareLines imgHls [((0, 0), (0, 50)), ((20, 0), (20, 50))] (560, 315)
--     print imgHls
--     print err

-- main = do
--     let imgHls = getHoughLines "data/road.jpg"
--     let err = compareLines imgHls [((0, 0), (0, 50)), ((20, 0), (20, 50))] (560, 315)
--     print imgHls
--     print err

main :: IO ()
main = trainModel
-- main :: IO Int32
-- main = testBedExample
