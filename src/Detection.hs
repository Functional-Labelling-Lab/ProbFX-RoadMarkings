{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Foreign.C.String ( newCString )
import CppFFI
  ( Scene(..),
    Camera(Camera, x, y, pitch, z, yaw, roll),
    findTextureDifference,
    getMeanPixelValue,
    testBed,
    setTargetImg,
    renderScene, getHoughLines, getSceneLines, Line )
import Foreign.C.String
import Foreign
    ( Storable(..), StablePtr(..), Int32, malloc, Storable(poke) )
import Foreign.Marshal.Alloc
import OpenSum (Member)
import System.IO.Unsafe ( unsafePerformIO )
import Hough (compareLines, quadError, compError)
import Foreign.C (CString)
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

clamp :: (Double, Double) -> Double -> Double
clamp (a, b) = min b . max a

type ErrorFunction = Scene -> Double

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


initRoadSample :: forall env sig m. 
  (Observables env '["x", "y", "z", "pitch", "yaw", "roll"] Double, Has (Model env) sig m) 
   => m Scene
initRoadSample = do
  x     <- uniform @env (-0.5) 0.5 #x
  y     <- uniform @env   0.05 0.5 #y
  z     <- uniform @env (-0.5) 0.5 #z
  pitch <- uniform @env (-0.2) 0.2 #pitch
  yaw   <- uniform @env (-0.2) 0.2 #yaw
  roll  <- uniform @env (-0.2) 0.2 #roll
  return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=0.0, yaw=0.0, roll=roll} }


roadGenerationModel :: forall env sig m.
  (Observables env ["x", "y", "z", "pitch", "yaw", "roll", "error"] Double, Has (Model env) sig m)
   => ErrorFunction
   -> m ()
roadGenerationModel errFun = do
  roadSample <- initRoadSample @env
  error <- normal @env (errFun roadSample) 50 #error
  return ()

trainModel :: ErrorFunction -> ([Env RoadEnv] -> IO ()) -> IO ()
trainModel errFun disp = do
  sampleIO $ do
    let mh_env :: Env RoadEnv
        mh_env =
            (#x := []) <:>
            (#y := []) <:>
            (#z := []) <:>
            (#pitch := []) <:>
            (#yaw := []) <:>
            (#roll := []) <:>
            (#error := repeat 0) <:>
            nil
    traceMHs <- mh iterations (roadGenerationModel @RoadEnv errFun) mh_env ["x", "y", "pitch", "roll"]
    liftS $ disp traceMHs
    return ()
  where
    iterations = 10


displayResults :: String -> [Env RoadEnv] -> IO ()
displayResults imgPath traceMHs = do
    x     <- dispVar "x    " #x
    y     <- dispVar "y    " #y
    pitch <- dispVar "pitch" #pitch
    z     <- dispVar "z    " #z
    yaw   <- dispVar "yaw  " #yaw
    roll  <- dispVar "roll " #roll
    error <- dispVar "error" #error
    testBed imgPath Scene {camera=Camera{x=x, y=y, z=z, pitch=pitch, yaw=yaw, roll=roll}}
    return ()
  where
    displayIters = [1..10]
    dispVar p x = do
      let xs = concatMap (get x) traceMHs
      putStrLn $ p ++ " = " ++ show (zip xs displayIters)
      return $ head xs


channelTrain :: String -> IO ()
channelTrain imagePath = do
  setTargetImg imagePath
  trainModel channelError (displayResults imagePath)

channelError :: ErrorFunction
channelError s = unsafePerformIO $ do
  scene <- malloc
  poke scene s
  fbo <- getSceneFBO
  renderScene scene fbo
  findTextureDifference 0
  first_error <- getMeanPixelValue 0
  findTextureDifference 1
  second_error <- getMeanPixelValue 1
  findTextureDifference 2
  third_error <- getMeanPixelValue 2
  print (first_error + second_error + third_error)
  return (first_error + second_error + third_error)


houghTrain :: String -> IO ()
houghTrain imagePath = do
  trainModel (houghError $ getHoughLines imagePath) (displayResults imagePath)

houghError :: [Line] -> ErrorFunction
houghError sceneLines scene = compareLines quadError sceneLines (getSceneLines scene ) (10, 10)


syntheticBenchmark :: ErrorFunction -> IO ()
syntheticBenchmark errFun = do
  args <- getArgs

  seed <- case map readMaybe args of
      [Just seed] -> return seed
      _ -> do
          seed <- randomIO
          putStrLn ("No seed provided, using seed " ++ show seed)
          return seed

  benchmark seed
  return ()
  
  where
    trainModelSeed :: Int -> Texture -> IO Scene
    trainModelSeed seed = sampleIOCustom seed . trainModelSampler

    trainModelSampler :: Texture -> Sampler Scene
    trainModelSampler texture = do
      traces <- mh 100 (roadGenerationModel @RoadEnv errFun) emptyRoadEnv ["x", "y", "z", "pitch", "yaw", "roll"]
      return $ fromJust $ envToScene $ head traces

    benchmark :: Int -> IO Double
    benchmark seed = do
      -- Create a scene to try and run the algorithm on.
      (scene, _) <- sampleIOCustom seed $ simulate emptyRoadEnv (initRoadSample @RoadEnv)

      -- Render this scene into an image
      targetTextureFBO <- createTextureFBO
      sceneMal <- malloc
      poke sceneMal scene 
      renderScene sceneMal (frameBuffer targetTextureFBO)
      free sceneMal

      -- Use mh to work backwards to the origional scene
      putStrLn ("Running algorithm on input seed..." ++ show seed)
      predictedScene <- trainModelSeed seed (texture targetTextureFBO)

      -- get accuracy
      let accuracy = sceneAccuracy scene predictedScene
      putStrLn $ "Scene:     " ++ show scene
      putStrLn $ "predicted: " ++ show predictedScene
      putStrLn ("FINAL ACCURACY: " ++ show accuracy)

      return accuracy
    
    -- TODO: Implement
    envToScene :: Env RoadEnv -> Maybe Scene
    envToScene env = run $ runObsReader env $ do
      -- mX     <- ask @RoadEnv #x
      -- mY     <- ask @RoadEnv #y
      -- mZ     <- ask @RoadEnv #z
      -- mPitch <- ask @RoadEnv #pitch
      -- mYaw   <- ask @RoadEnv #yaw
      -- mRoll  <- ask @RoadEnv #roll
      -- mError <- ask @RoadEnv #error

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

    sceneAccuracy :: Scene -> Scene -> Double
    -- POST: 0 <= sceneAccuracy s s' <= 1
    sceneAccuracy scene scene' = 1 - normalise (0, sqrt n) (euclidian sv (sceneToVec scene'))
      where
        n  = fromIntegral $ length sv
        sv = sceneToVec scene

    normalise :: (Double, Double) -> Double -> Double
    -- PRE: lower <= value <= upper
    -- POST: 0 <= normalise (lower, upper) value <= 1
    -- POST: a < b => normalise (l, u) a < normalise (l, u) b
    normalise (lower, upper) value = (value - lower) / (upper - lower)

    euclidian :: Floating a => [a] -> [a] -> a
    euclidian = (sqrt . sum) ... zipWith (join (*) ... subtract)
      where
        (...) = (.)(.)(.)
    
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


main :: IO ()
-- main = houghTrain "data/real_road.jpg"
-- main = trainModel "data/read_road.jpg" channelError
main = syntheticBenchmark channelError
