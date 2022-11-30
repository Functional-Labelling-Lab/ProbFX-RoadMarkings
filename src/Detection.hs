{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Model ( Model, normal, uniform )
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import Inference.MH ( mh, mhRaw )
import Sampler ( sampleIO, liftS )
import Control.Algebra (Has)

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
import Foreign
    ( Storable(..), StablePtr(..), Int32, malloc, Storable(poke) )
import Foreign.Marshal.Alloc ( malloc )
import OpenSum (Member)
import System.IO.Unsafe ( unsafePerformIO )
import Hough (compareLines, quadError, compError)
import Foreign.C (CString)

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
    traceMHs <- mh 1000000 (roadGenerationModel @RoadEnv errFun) mh_env ["x", "y", "pitch", "roll"]
    liftS $ disp traceMHs
    return ()

displayResults :: CString -> [Env RoadEnv] -> IO ()
displayResults imgPath traceMHs = do
    x     <- dispVar "x:    " #x
    y     <- dispVar "y:    " #y
    pitch <- dispVar "pitch:" #pitch
    z     <- dispVar "z:    " #z
    yaw   <- dispVar "yaw:  " #yaw
    roll  <- dispVar "roll: " #roll
    error <- dispVar "error:" #error
    testBed imgPath x y z pitch 0.0 roll
    return ()
  where
    dispVar p x = do
      let xs = concatMap (get x) traceMHs
      putStrLn $ p ++ show (take 10 xs)
      return (head xs)

{-
BUG: Scene passed to testbed, road does not match lines provided by getSceneLines

Python plot:
https://trinket.io/python3/65b7ca4a50

The following code demonstrates this. There is a change (see line 354 in render.cpp)
-}

main :: IO ()
-- main = houghTrain "data/real_road.jpg"
main = do
  let scene = Camera {x = -4.157737428065356e-3, y = 0.33617560076802844, z = 0.0, pitch = 5.633504256040617e-3, yaw = 0.0, roll = -0.18965620426531105}
  imgPath <- newCString "data/real_road.jpg"
  let s = Scene {camera = scene}
  print $ getSceneLines s
  testBed imgPath (x scene) (y scene) (z scene) (pitch scene) (yaw scene) (roll scene)
  return ()


channelTrain :: String -> IO ()
channelTrain imagePath = do
  imgPath <- newCString imagePath
  setTargetImg imgPath
  trainModel channelError (displayResults imgPath)
  where
    channelError :: ErrorFunction
    channelError s = unsafePerformIO $ do
      scene <- malloc
      poke scene s
      renderScene scene
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
  imgPath <- newCString imagePath
  trainModel (houghError $ getHoughLines imagePath) (displayResults imgPath)
  where
    houghError :: [Line] -> ErrorFunction
    houghError sceneLines scene = unsafePerformIO $ do
      let lines = getSceneLines scene 
      let err = compareLines quadError sceneLines (lines) (10, 10)
      -- putStrLn $ "Lines: " ++ show lines ++ " error:" ++ show err ++ "scene: " ++ show scene
      return err
