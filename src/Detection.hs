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
    renderScene )
import Foreign
    ( Storable(..), StablePtr(..), Int32, malloc, Storable(poke) )
import Foreign.Marshal.Alloc ( malloc )
import OpenSum (Member)
import System.IO.Unsafe ( unsafePerformIO )
import Hough (compareLines)

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
  return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=z, yaw=0.0, roll=roll} }


roadGenerationModel :: forall env sig m. 
  (Observables env ["x", "y", "z", "pitch", "yaw", "roll", "error"] Double, Has (Model env) sig m) 
   => ErrorFunction 
   -> m ()
roadGenerationModel errFun = do
  roadSample <- initRoadSample @env
  error <- normal @env (errFun roadSample) 50 #error
  return ()

trainModel :: String -> ErrorFunction -> IO ()
trainModel imagePath errFun = do
  imgPath <- newCString imagePath
  setTargetImg imgPath

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

    traceMHs <- mh 1000 (roadGenerationModel @RoadEnv errFun) mh_env ["x", "y", "z", "pitch", "roll"]

    -- get resulting traces for parameters
    let getvar x = concatMap (get x) traceMHs

    let xs      = getvar #x
    let ys      = getvar #y
    let pitches = getvar #pitch
    let zs      = getvar #z
    let yaws    = getvar #yaw
    let rolls   = getvar #roll
    let errors  = getvar #error

    disp xs
    disp ys
    disp pitches
    disp zs
    disp yaws
    disp rolls    
    liftS $ print =<< testBed imgPath (head xs) (head ys) (head zs) (head pitches) 0.0 (head rolls)
    disp errors
    liftS $ print $ length xs
    liftS $ print (fromIntegral (length xs) / 100.0)
  where
    disp = liftS . print . take 10


main :: IO ()
-- main = trainModel "data/read_road.jpg" channelError
main = do
  imgPath <- newCString "data/real_road.jpg"
  testBed imgPath 0.11319984526740867 0.3784490271439612 0.0 (-0.1) 0.0 0.0
  return ()

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
