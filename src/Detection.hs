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

    
xRange, yRange, zRange, pitchRange, yawRange, rollRange :: (Double, Double)
xRange = (-0.5, 0.5)
yRange = (0.05, 0.5)
zRange = (-0.5, 0.5)
pitchRange = (-0.2, 0.2)
yawRange = (-0.2, 0.2)
rollRange = (-0.2, 0.2)

emptyRoadEnv :: Env RoadEnv
emptyRoadEnv = #x := [] <:> #y := [] <:> #z := [] <:> #pitch := [] <:> #yaw := [] <:> #roll := [] <:> #error := repeat 0 <:> nil


initRoadSample :: forall env sig m. 
  (Observables env '["x", "y", "z", "pitch", "yaw", "roll"] Double, Has (Model env) sig m) 
   => m Scene
initRoadSample = do
  x     <- uniform @env (fst xRange) (snd xRange) #x
  y     <- uniform @env (fst yRange) (snd yRange) #y
  z     <- uniform @env (fst zRange) (snd zRange) #z
  pitch <- uniform @env (fst pitchRange) (snd pitchRange) #pitch
  yaw   <- uniform @env (fst yawRange) (snd yawRange) #yaw
  roll  <- uniform @env (fst rollRange) (snd rollRange) #roll
  return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=z, yaw=0.0, roll=roll} }


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
      putStrLn $ "FINAL ACCURACY: " ++ show accuracy

      return accuracy
    
    -- TODO: Implement
    envToScene :: Env RoadEnv -> Maybe Scene
    envToScene env = run $ runObsReader env $ do
      mX     <- ask @RoadEnv #x
      mY     <- ask @RoadEnv #y
      mZ     <- ask @RoadEnv #z
      mPitch <- ask @RoadEnv #pitch
      mYaw   <- ask @RoadEnv #yaw
      mRoll  <- ask @RoadEnv #roll

      return $ case (mX, mY, mZ, mPitch, mYaw, mRoll) of
        (Just x, Just y, Just z, Just pitch, Just yaw, Just roll) -> Just $ Scene {
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
    sceneAccuracy scene scene' = 1 - normalise (0, sqrt n) (euclidian sv sv')
      where
        n  = fromIntegral $ length sv
        sv = sceneToVec scene
        sv' = sceneToVec scene'

    normalise :: (Double, Double) -> Double -> Double
    -- PRE: lower <= value <= upper
    -- POST: 0 <= normalise (lower, upper) value <= 1
    -- POST: a < b => normalise (l, u) a < normalise (l, u) b
    normalise (lower, upper) value = (value - lower) / (upper - lower)

    euclidian :: Floating a => [a] -> [a] -> a
    euclidian xs ys = sqrt $ sum $ zipWith (\x y -> (x - y) ^ 2) xs ys

    sceneToVec :: Scene -> [Double]
    sceneToVec scene = zipWith normalise
      [xRange, yRange, zRange, pitchRange, yawRange, rollRange]
      (map ($ camera scene) [x, y, z, pitch, yaw, roll])
  
main :: IO ()
-- main = houghTrain "data/real_road.jpg"
-- main = trainModel "data/read_road.jpg" channelError
main = syntheticBenchmark channelError
