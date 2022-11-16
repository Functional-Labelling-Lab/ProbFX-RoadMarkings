--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Main (initRoadSample, main, trainModel) where

import Debug.Trace ( trace )
import Model ( Model, normal, uniform )
import System.Environment ( getArgs )
import Prog ( call )
import Effects.ObsReader ( ObsReader(Ask) )
import Model ( Model(Model), normal, uniform )
import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
import Effects.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import Inference.MH ( mh, mhRaw )
import Sampler ( sampleIO, liftS )
import Control.Algebra (Has)

import Data.List (partition)
import Foreign.C.String
import CppFFI
import Foreign
    ( Storable(..), StablePtr(..), Int32, malloc, Storable(poke) )
import Foreign.Marshal.Alloc
import OpenSum (Member)
import System.IO.Unsafe
import Hough (compareLines)

clamp :: (Double, Double) -> Double -> Double
clamp (a, b) = min b . max a


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

initRoadSample :: forall env sig m. (Observables env '["x", "y", "pitch", "yaw", "roll"] Double, Has (Model env) sig m) => m Scene
initRoadSample = do
    x <- uniform @env (-1) 2 #x
    y <- uniform @env (-1) 2 #y
    -- z <- uniform @env (-0.01) 0.01 #z
    pitch <- normal @env 0 0.3 #pitch
    -- yaw <- normal @env 0.2 0.3 #yaw
    -- roll <- normal @env 0.2 0.3 #roll

    return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=0, yaw=0, roll=0} }

roadGenerationModel :: forall env sig m. (Observables env ["x", "y", "pitch", "yaw", "roll", "error"] Double, Has (Model env) sig m) => m ()
roadGenerationModel = do
    roadSample <- initRoadSample @env
    error <- normal @env (errorFunction roadSample) 6 #error
    return ()

--- Main code

--- Do not use this in non-thread-safe code, please 

errorFunction :: Scene -> Double
errorFunction s = unsafePerformIO $ do
    scene <- malloc
    poke scene s

    renderScene scene
    findTextureDifference
    realToFrac <$> getMeanPixelValue


testBedExample :: IO Int32
testBedExample = testBed 0.2 0.2 0 (-0.0) 0 0

--- Run training loop
trainModel :: IO ()
trainModel = do
    string <- newCString "backend/src/textures/rendered_road.jpg"
    setTargetImg string

    sampleIO $ do

        let mh_env :: Env RoadEnv
            mh_env = (#x := []) <:> (#y := []) <:> (#z := []) <:> (#pitch := []) <:> (#yaw := []) <:> (#roll := []) <:> (#error := repeat 0) <:> nil

        traceMHs <- mh 100 (roadGenerationModel @RoadEnv) mh_env ["x", "y", "pitch"]

        let xs = concatMap (get #x) traceMHs
        let ys = concatMap (get #y) traceMHs
        let pitches = concatMap (get #pitch) traceMHs
        

        liftS $ print $ head xs
        liftS $ print $ head ys
        liftS $ print $ head pitches


main :: IO ()
-- main = trainModel
main = do
    let imgHls = getHoughLines "data/road.jpg"
    let err = compareLines imgHls [((0, 0), (0, 50)), ((20, 0), (20, 50))] (560, 315)
    print imgHls
    print err

-- main :: IO Int32
-- main = testBedExample
