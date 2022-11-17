--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE TypeOperators #-}

module Main (initRoadSample, main, trainModel) where

import Debug.Trace ( trace )
import System.Environment ( getArgs )
import Data.Kind (Constraint)
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import Control.Algebra (Has)

import CppFFI (Scene (..), Camera (..), Texture,
               getSceneFBO, renderScene, findTextureDifference,
               getMeanPixelValue, testBed, setTargetImg, createTextureFBO,
               TextureFBO (frameBuffer, texture), getTargetTexture, getHoughLines)

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
import Control.Algebra (run)
import Data.Maybe (fromJust)

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
    mY <- ask @RoadEnv #y
    -- let mZ = Just 0
    mZ <- ask @RoadEnv #z
    mPitch <- ask @RoadEnv #pitch
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

clamp :: (Double, Double) -> Double -> Double
clamp (a, b) = min b . max a

initRoadSample :: forall sig m. (Has (Model RoadEnv) sig m) => m Scene
initRoadSample = do
    x <- uniform @RoadEnv (-1) 2 #x
    y <- uniform @RoadEnv (-1) 2 #y
    z <- uniform @RoadEnv (-0.01) 0.01 #z
    pitch <- normal @RoadEnv 0 0.3 #pitch
    -- yaw <- normal @env 0.2 0.3 #yaw
    -- roll <- normal @env 0.2 0.3 #roll

    return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=z, yaw=0, roll=0} }

roadGenerationModel :: forall sig m. (Has (Model RoadEnv) sig m) => m ()
roadGenerationModel = do
    roadSample <- initRoadSample
    error <- normal @RoadEnv (errorFunction roadSample) 6 #error
    return ()

--- Main code

--- Do not use this in non-thread-safe code, please 

errorFunction :: Scene -> Double
errorFunction scene = unsafePerformIO $ do
    sceneFBO <- getSceneFBO -- returns global sceneFBO
    renderScene scene sceneFBO

    findTextureDifference -- implicitly uses global sceneFBO
    getMeanPixelValue

testBedExample :: IO Int32
testBedExample = testBed 0.2 0.2 0 (-0.0) 0 0

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

benchmark :: Int -> IO ()
benchmark seed = do
    -- Create a scene to try and run the algorithm on.
    (scene, _) <- sampleIOCustom seed $ simulate emptyRoadEnv initRoadSample

    -- Render this scene into an image
    targetTextureFBO <- createTextureFBO
    renderScene scene (frameBuffer targetTextureFBO)

    -- Use mh to work backwards to the origional scene
    predictedScene <- trainModelSeed seed (texture targetTextureFBO)

    print scene
    print predictedScene
    
    -- return (undefined scene predictedScene)

-- Scene {camera = Camera {x = 0.6879765442476931, y = 0.10672289591094897, z = 6.263997681267841e-4, pitch = 0.30004201200001984, yaw = 0.0, roll = 0.0}}
-- Scene {camera = Camera {x = 1.9423491560450756e-2, y = 0.13973177834102612, z = 6.743796537081953e-3, pitch = -0.20179312329488874, yaw = 0.0, roll = 0.

main :: IO ()
-- main = trainModelFromFile "data/road.jpg" >>= print
main = benchmark 42
-- main = do
--     let imgHls = getHoughLines "data/road.jpg"
--     let err = compareLines imgHls [((0, 0), (0, 50)), ((20, 0), (20, 50))] (560, 315)
--     print imgHls
--     print err

-- main :: IO Int32
-- main = testBedExample
