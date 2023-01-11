--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}

--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use fewer imports" #-}
import Model ( Model, normal, uniform, normal' )
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import Inference.MH ( mh, mhRaw )
import Sampler ( sampleIO, liftS, Sampler, sampleIOCustom )
import Control.Algebra (Has)
import System.Console.CmdArgs(CmdArgs, (&=), def, help, opt, modes, summary, cmdArgs, cmdArgsMode, Mode, program, cmdArgsRun)

import Data.List (partition)
import Foreign.C.String
import CppFFI
import Foreign
    ( Storable(..), StablePtr(..), Int32, malloc, Storable(poke) )
import Foreign.Marshal.Alloc
import System.IO.Unsafe ( unsafePerformIO )
import Hough ( compareLines, compareLines )
import Foreign.C (CString)
import System.IO.Unsafe
import Model (Model, uniform, normal)
import Sampler (sampleIO, liftS, sampleIOCustom, Sampler)
import Inference.SIM (simulate)
import Inference.MH (mh)
import Control.Effect.ObsReader (ask)
import Control.Carrier.ObsReader (runObsReader)
import Control.Algebra (run, Has)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Control.Monad (join, zipWithM)
import System.Console.CmdArgs.Implicit (Data)
import System.Console.CmdArgs (Typeable)
import System.Directory.Internal.Prelude (replicateM)
import System.Directory (getDirectoryContents)
import GHC.OldList (zip4)

clamp :: (Double, Double) -> Double -> Double
clamp (a, b) = min b . max a


--- Probabilistic Model


--- Training
type RoadEnv =
 '[ "x"         := Double
  , "y"         := Double
  , "z"         := Double
  , "pitch"     := Double
  , "roll"      := Double
  , "yaw"      := Double
  , "error"     := Double
 ]

xRange, yRange, zRange, pitchRange, yawRange, rollRange :: (Double, Double)
xRange = (0, 0.05)
yRange = (0.15, 0.05)
zRange = (0, 0.05)
pitchRange = (0, 0.05)
yawRange = (0, 0.05)
rollRange = (0, 0.05)

emptyRoadEnv :: Env RoadEnv
emptyRoadEnv = #x := [] <:> #y := [] <:> #z := [] <:> #pitch := [] <:> #roll := [] <:> #yaw := [] <:> #error := repeat 0 <:> nil


initRoadSample :: (Observables env '["x", "y", "z", "pitch", "roll", "yaw"] Double) => Model env sig m Scene
initRoadSample = do
  x <- uniform (-0.5) 0.5 #x
  y <- uniform 0.05 0.5 #y
  z <- uniform (-0.5) 0.5 #z
  pitch <- uniform (-0.3) 0.3 #pitch
  yaw <- uniform (-0.3) 0.3 #yaw
  roll <- uniform (-0.3) 0.3 #roll
  return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=z, yaw=yaw, roll=roll} }


roadGenerationModel :: (Observables env ["x", "y", "z", "pitch", "roll", "yaw", "error"] Double) => Model env sig m ()
roadGenerationModel = do
    roadSample <- initRoadSample
    error <- normal (errorFunction roadSample) 20 #error
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

    return (first_error + second_error + third_error)


testBedExample :: IO Int32
testBedExample = testBed 0.11319984526740867 0.3784490271439612 0.0 (-0.1) 0.0 0.0

sampleNormal :: Double -> Model env sig m Double
sampleNormal mean = normal' mean 0.1

--- Run training loop
trainModel :: FilePath -> Int -> IO (Scene, Double)
trainModel image seed = do
    print image
    string <- newCString image
    setTargetImg string

    sampleIOCustom seed $ do

        let mh_env :: Env RoadEnv
            mh_env = (#x := []) <:> (#y := []) <:> (#z := []) <:> (#pitch := []) <:> (#roll := []) <:> (#yaw := []) <:> (#error := repeat 0) <:> nil

        -- let mhTrans :: Env RoadEnv
        -- let mhTrans = (#x := sampleNormal) <:> (#y := sampleNormal) <:> (#z := sampleNormal) <:> (#pitch := sampleNormal) <:> (#roll := sampleNormal) <:> nil

        traceMHs <- mh 500 (roadGenerationModel @RoadEnv) mh_env nil nil

        let xs = concatMap (get #x) traceMHs
        let ys = concatMap (get #y) traceMHs
        let pitches = concatMap (get #pitch) traceMHs
        let zs = concatMap (get #z) traceMHs
        let rolls = concatMap (get #roll) traceMHs
        let yaws = concatMap (get #yaw) traceMHs

        return (Scene {camera = Camera {x=head xs, y=head ys, pitch=head pitches, z=head zs, yaw=head yaws, roll=head rolls}}, 0.0)

data Args = Benchmark { outputPath :: String, seed :: Int, runs :: Int }
        | Run { inputPath :: String, outputPath :: String } deriving (Show, Data, Typeable)

inputPathMsg x = x &= help "Input path folder" &= opt "input"
outputPathMsg x = x &= help "Output path folder" &= opt "output"
seedMsg x = x &= help "Random seed for benchmark" &= opt "seed"
runsMsg x = x &= help "Number of runs for benchmark" &= opt "runs"

syntheticBenchmark :: String -> Int -> Int -> IO (Scene, Scene, Double)
syntheticBenchmark output run seed = do

  benchmark run seed

  where

    benchmark :: Int -> Int -> IO (Scene, Scene, Double)
    benchmark seed run = do
      -- Create a scene to try and run the algorithm on.
      (scene, _) <- sampleIOCustom (seed + run) $ simulate emptyRoadEnv (initRoadSample @RoadEnv)

      let name = output ++ "/" ++ show run ++ ".png"
      saveScenes output [("/" ++ show run ++ ".png", scene)]

      -- Render this scene into an image
      sceneMal <- malloc
      poke sceneMal scene
      renderScene sceneMal
      free sceneMal

      -- Use mh to work backwards to the origional scene
      (predictedScene, _) <- trainModel name (seed + 21577)

      -- get accuracy
      let accuracy = sceneAccuracy scene predictedScene
      saveScenes output [("/" ++ show run ++ "_out.png", predictedScene)]

      return (scene, predictedScene, accuracy)

    -- TODO: Implement
    -- envToScene :: Env RoadEnv -> Maybe Scene
    envToScene env = Control.Algebra.run $ runObsReader env $ do
      mX     <- ask @RoadEnv #x
      mY     <- ask @RoadEnv #y
      mZ     <- ask @RoadEnv #z
      mPitch <- ask @RoadEnv #pitch
      mRoll  <- ask @RoadEnv #roll

      return $ case (mX, mY, mZ, mPitch, mRoll) of
        (Just x, Just y, Just z, Just pitch, Just roll) -> Just $ Scene {
          camera = Camera {
              x = x,
              y = y,
              z = z,
              pitch = pitch,
              yaw = 0.0,
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


serializeBenchmarks :: [(String, Scene, Scene, Double)] -> String
serializeBenchmarks results = concat outStr
    where
        lines = map (\(name, Scene {
            camera = Camera { x = target_x, y = target_y, z = target_z,  yaw = target_yaw, pitch = target_pitch, roll = target_roll}
        }, Scene {
            camera = Camera { x = generated_x, y = generated_y, z = generated_z,  yaw = generated_yaw, pitch = generated_pitch, roll = generated_roll}
            }, error) ->
                name ++ "," ++ show target_x ++ "," ++ show target_y ++ "," ++ show target_z ++ "," ++ show target_pitch ++ "," ++ show target_yaw ++ "," ++ show target_roll ++ ","
                ++ show generated_x ++ "," ++ show generated_y ++ "," ++ show generated_z ++ "," ++ show generated_pitch ++ "," ++ show generated_yaw ++ "," ++ show generated_roll ++ "," ++ show error ++ "\n") results
        outStr = "name,target_x,target_y,target_z,target_pitch,target_yaw,target_roll,generated_x,generated_y,generated_z,generated_pitch,generated_yaw,generated_roll,error\n" : lines



runBenchmark :: String -> Int -> Int -> IO ()
runBenchmark output seed runs = do
    putStrLn "Benchmarking"
    results <- mapM (syntheticBenchmark output seed) [0..runs]
    let (scenes, predictedScenes, accuracies) = unzip3 results

    let outputString = serializeBenchmarks (zip4 (map show [0..runs]) scenes predictedScenes accuracies)

    writeFile (output ++ "/results.csv") outputString



    return ()


runRoadMarkings :: String -> String -> IO ()
runRoadMarkings input output  = do
    putStrLn "Running"
    print input
    files <- getImagesInPath input
    let filePaths = map (\x -> input ++ "/" ++ x) files

    parameters <- mapM (`trainModel` 0) filePaths
    let (params, errors) = unzip parameters

    let zipped = zip3 files params errors

    saveScenes output (zip files params)

    let outputString = serializeResults zipped

    writeFile (output ++ "/results.csv") outputString




serializeResults :: [(String, Scene, Double)] -> String
serializeResults results = concat outStr
    where
        lines = map (\(name, Scene {camera = Camera { x, y, z, yaw, pitch, roll}}, error) -> name ++ "," ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show pitch ++ "," ++ show yaw ++ "," ++ show roll ++ "," ++ show error ++ "\n") results
        outStr = "name,x,y,z,pitch,yaw,roll,error\n" : lines



saveScenes :: String -> [(String, Scene)] -> IO ()
saveScenes path files = do
    let (strings, scenes) = unzip files
    cstrs <- mapM (\x -> newCString (path ++ "/" ++ x)) strings
    ptrs <- replicateM (length cstrs) malloc
    scenePtrs <- zipWithM poke ptrs scenes

    mapM_ (\(x, y) -> saveScene y x) (zip cstrs ptrs)

getImagesInPath :: FilePath -> IO [FilePath]
getImagesInPath name = do
    files <- getDirectoryContents name
    let images = filter (\x -> take 4 (reverse x) == "gnp." || take 4 (reverse x) == "gpj." || take 5 (reverse x) == "gepj." ) files
    return images

execute :: Args -> IO ()
execute (Benchmark outputPath seed runs) = runBenchmark outputPath seed runs
execute (Run inputPath outputPath) = runRoadMarkings inputPath outputPath

mode :: Mode (CmdArgs Args)
mode = cmdArgsMode $ modes [benchmark,run] &= help "Build helper program" &= program "maker" &= summary "Maker v1.0\nMake it"
    where
        benchmark = Benchmark { outputPath = outputPathMsg def, seed = seedMsg def, runs = runsMsg def }
        run = Run { inputPath = inputPathMsg def, outputPath = outputPathMsg def }

main :: IO ()
main = do
    args <- cmdArgsRun mode
    execute args

-- main :: IO Int32
-- main = saveScenes "./" [("0.png", Scene {camera = Camera {x = 0.07, y = 0.15, z = 0.1, yaw = 0.05, pitch = -0.15, roll = 0.0}})]


-- Functions:
-- 1. Run RoadMarkings on a folder
-- 2. Run benchmarking on generated images
-- Command line arguments
-- 1. {executable} benchmark {output} # runs benchmarking on generated images and saves the output to the specified json file
-- 2. {executable} run {path} {output} # runs RoadMarkings on a folder of images and saves the output to the specified json file
-- 3. {executable} 

-- name,(params,)*,error,execution time (s)
-- "image1.png",(params,)*,3000,40