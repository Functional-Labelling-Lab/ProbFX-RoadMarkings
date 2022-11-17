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
import Inference.SIM ( runSimulate, simulate )
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

-- TODO: Implement
envToScene :: Env '[ "y" ':= Double, "roadWidth" ':= Double] -> Scene
envToScene = undefined

sceneToEnv :: Scene -> Env '[ "y" ':= Double, "roadWidth" ':= Double]
sceneToEnv = undefined

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

    sceneFBO <- getSceneFBO -- returns global sceneFBO
    renderScene scene sceneFBO

    findTextureDifference -- implicitly uses global sceneFBO



testBedExample :: IO Int32
testBedExample = testBed 0.2 0.2 0 (-0.0) 0 0

--- Run training loop on the image stored in frameBuffer
trainModel :: Texture -> IO Scene
trainModel texture = sampleIO $ do
    let mh_env :: Env RoadEnv
        mh_env = (#x := []) <:> (#y := []) <:> (#z := []) <:> (#pitch := []) <:> (#yaw := []) <:> (#roll := []) <:> (#error := repeat 0) <:> nil
    traceMHs <- mh 100 (roadGenerationModel @RoadEnv) ((), mh_env) ["x", "y", "pitch"]

    -- TODO: remove error from finalTrace
    let finalTrace = last traceMHs

    return $ envToScene finalTrace

trainModelFromFile :: String -> IO Scene
trainModelFromFile path = do
    setTargetImg path -- reads file in path to targetTexture

    targetTexture <- getTargetTexture
    trainModel targetTexture

benchmark :: IO ()
benchmark = do
    -- Create a scene to try and run the algorithm on.
    -- TODO: use sampleIOCustom instead of sampleIO so seed can be provided.
    let env = (#y := []) <:> (#roadWidth := []) <:> nil
    (scene, _) <- sampleIO $ simulate (const initRoadSample) env undefined

    -- Render this scene into an image
    -- TODO: I don't think rendering should require IO, so maybe make it not
    --       I think it should take a buffer as input and write it to that
    --       or take a buffer as input idk
    targetTextureFBO <- createTextureFBO
    s <- malloc
    poke s scene
    renderScene s (frameBuffer targetTextureFBO)

    -- Use mh to work backwards to the origional scene
    -- TODO: trainModel needs to be altered to take all the above stuff as input,
    --       and return the predicted scene
    predictedScene <- trainModel (texture targetTextureFBO)

    -- TODO: replace undefined with some diff function which says how close
    --       to the input parameters we got.
    return (undefined scene predictedScene)


main :: IO ()
-- main = trainModel
main = do
    let imgHls = getHoughLines "data/road.jpg"
    let err = compareLines imgHls [((0, 0), (0, 50)), ((20, 0), (20, 50))] (560, 315)
    print imgHls
    print err

-- main :: IO Int32
-- main = testBedExample
