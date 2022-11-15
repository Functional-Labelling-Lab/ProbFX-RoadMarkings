--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedLabels #-}

import Debug.Trace ( trace )

import System.Environment ( getArgs )
import Prog ( call )
import Effects.ObsReader ( ObsReader(Ask) )
import Model ( Model(Model), normal, uniform )
import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
import Effects.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import Inference.SIM ( runSimulate )
import Inference.MH ( mh )
import Sampler ( sampleIO, liftS )

import Data.List (partition)
import Foreign
import Foreign.C.String
import CppFFI
import Foreign (Storable(..), StablePtr(..))
import Foreign
import Foreign.Marshal.Alloc
import Effects.Lift (Lift)
import OpenSum (Member)
import System.IO.Unsafe

clamp :: (Double, Double) -> Double -> Double
clamp (a, b) = (min b) . max a


--- Training

initRoadSample :: (Observables env ["y", "roadWidth"] Double) => Model env es Scene
initRoadSample = do
    -- x <- uniform (-0.5) 0.5 #x
    -- 0.6, 0, 0.2, 0
    y <- uniform 0.0 0.4 #y
    -- z <- uniform (-0.01) 0.01 #z
    -- pitch <- uniform (-0.3) 0.3 #pitch
    -- yaw <- uniform (-0.01) 0.01 #yaw
    -- roll <- uniform (-0.01) 0.01 #roll
    roadWidth <- uniform 0.0 0.5 #roadWidth

    return $ Scene { camera = Camera {x=0.2, y=y, pitch=(-0.1), z=0, yaw=0, roll=0} }

roadGenerationModel :: (Observables env ["y", "roadWidth", "error"] Double) => () -> Model env es ()
roadGenerationModel _ = do
    roadSample <- initRoadSample
    error <- normal (errorFunction roadSample) 10 #error
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
testBedExample = testBed 0.2 0.2 0 (-0.1) 0 0 0.3

--- Run training loop
trainModel :: IO ()
trainModel = do
    string <- newCString "/src/textures/rendered_road.jpg"
    setTargetImg string
    
    sampleIO $ do

        let mh_env = (#y := []) <:> (#roadWidth := []) <:> (#error := repeat 0) <:> nil
        traceMHs <- mh 5000 roadGenerationModel ((), mh_env) ["y", "roadWidth"]
        
        let ys = concatMap (get #y) traceMHs

        liftS $ print ys

        liftS $ print $ concatMap (get #roadWidth) traceMHs


main :: IO ()
main = trainModel

-- main :: IO Int32
-- main = testBedExample
