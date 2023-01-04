--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import Model ( Model, normal, uniform )
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

initRoadSample :: forall env sig m. (Observables env '["x", "y", "z", "pitch", "yaw", "roll"] Double, Has (Model env) sig m) => m Scene
initRoadSample = do
    x <- uniform @env (-0.5) 0.5 #x
    y <- uniform @env (0.05) 0.5 #y
    z <- uniform @env (-0.5) 0.5 #z
    pitch <- uniform @env (-0.3) 0.3 #pitch
    yaw <- uniform @env (-0.3) 0.3 #yaw
    roll <- uniform @env (-0.3) 0.3 #roll

    return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=z, yaw=yaw, roll=roll} }

roadGenerationModel :: forall env sig m. (Observables env ["x", "y", "z", "pitch", "yaw", "roll", "error"] Double, Has (Model env) sig m) => m ()
roadGenerationModel = do
    roadSample <- initRoadSample @env
    error <- normal @env (errorFunction roadSample) 8 #error
    return ()

--- Main code

--- Do not use this in non-thread-safe code, please 

errorFunction :: Scene -> Double
errorFunction s = unsafePerformIO $ do
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


testBedExample :: IO Int32
testBedExample = testBed (0.11319984526740867) (0.3784490271439612) (0.0) (-0.1) (0.0) (0.0)

--- Run training loop
trainModel :: IO ()
trainModel = do
    string <- newCString "backend/src/images/real_road.jpg"
    setTargetImg string

    sampleIO $ do

        let mh_env :: Env RoadEnv
            mh_env = (#x := []) <:> (#y := []) <:> (#z := []) <:> (#pitch := []) <:> (#yaw := []) <:> (#roll := []) <:> (#error := repeat 0) <:> nil

        traceMHs <- mh 500 (roadGenerationModel @RoadEnv) mh_env ["x", "y", "z", "pitch", "roll", "yaw"]

        let xs = concatMap (get #x) traceMHs
        let ys = concatMap (get #y) traceMHs
        let pitches = concatMap (get #pitch) traceMHs
        let zs = concatMap (get #z) traceMHs
        let yaws = concatMap (get #yaw) traceMHs
        let rolls = concatMap (get #roll) traceMHs
        let errors = concatMap (get #error) traceMHs
        

        liftS $ print $ take 10 xs
        liftS $ print $ take 10 ys
        liftS $ print $ take 10 pitches
        liftS $ print $ take 10 zs
        liftS $ print $ take 10 yaws
        liftS $ print $ take 10 rolls

        liftS $ print =<< (testBed (head xs) (head ys) (head zs) (head pitches) (0.0) (head rolls)) 
        -- liftS $ print $ take 10 errors
        -- liftS $ print $ (length xs)
        -- liftS $ print $ ((fromIntegral (length xs)) / 100.0)


-- main = do
--     let imgHls = getHoughLines "data/road.jpg"
--     let err = compareLines imgHls [((0, 0), (0, 50)), ((20, 0), (20, 50))] (560, 315)
--     print imgHls
--     print err

main :: IO ()
main = trainModel
-- main :: IO Int32
-- main = testBedExample
