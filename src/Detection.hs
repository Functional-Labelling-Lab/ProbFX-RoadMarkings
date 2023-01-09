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

import Model ( Model, normal, uniform )
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import Inference.MH ( mh, mhRaw )
import Sampler ( sampleIO, liftS )
import Control.Algebra (Has)
import System.Console.CmdArgs(CmdArgs, (&=), def, help, opt, modes, summary, cmdArgs, cmdArgsMode, Mode, program, cmdArgsRun)

import Data.List (partition)
import Foreign.C.String
import CppFFI
import Foreign
    ( Storable(..), StablePtr(..), Int32, malloc, Storable(poke) )
import Foreign.Marshal.Alloc
import OpenSum (Member)
import System.IO.Unsafe
import System.Directory(getDirectoryContents)
import Hough (compareLines)
import Data.Data
import Control.Monad (replicateM, zipWithM)

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

xRange, yRange, zRange, pitchRange, yawRange, rollRange :: (Double, Double)
xRange = (-0.5, 0.5)
yRange = (0.05, 0.5)
zRange = (-0.5, 0.5)
pitchRange = (-0.2, 0.2)
yawRange = (-0.2, 0.2)
rollRange = (-0.2, 0.2)

emptyRoadEnv :: Env RoadEnv
emptyRoadEnv = #x := [] <:> #y := [] <:> #z := [] <:> #pitch := [] <:> #yaw := [] <:> #roll := [] <:> #error := repeat 0 <:> nil


initRoadSample :: forall env sig m. (Observables env '["x", "y", "z", "pitch", "yaw", "roll", "error"] Double, Has (Model env) sig m) => m Scene
initRoadSample = do
  x     <- uniform @env (fst xRange) (snd xRange) #x
  y     <- uniform @env (fst yRange) (snd yRange) #y
  z     <- uniform @env (fst zRange) (snd zRange) #z
  pitch <- uniform @env (fst pitchRange) (snd pitchRange) #pitch
  yaw   <- uniform @env (fst yawRange) (snd yawRange) #yaw
  roll  <- uniform @env (fst rollRange) (snd rollRange) #roll
  return $ Scene { camera = Camera {x=x, y=y, pitch=pitch, z=z, yaw=0.0, roll=roll} }


roadGenerationModel :: forall env sig m. (Observables env ["x", "y", "z", "pitch", "yaw", "roll", "error"] Double, Has (Model env) sig m) => m ()
roadGenerationModel = do
    roadSample <- initRoadSample @env
    error <- normal @env (errorFunction roadSample) 50 #error
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

--- Run training loop
trainModel :: FilePath -> IO (Scene, Double)
trainModel image = do
    print image
    string <- newCString image
    setTargetImg string

    sampleIO $ do

        let mh_env :: Env RoadEnv
            mh_env = (#x := []) <:> (#y := []) <:> (#z := []) <:> (#pitch := []) <:> (#yaw := []) <:> (#roll := []) <:> (#error := repeat 0) <:> nil

        traceMHs <- mh 1000 (roadGenerationModel @RoadEnv) mh_env ["x", "y", "z", "pitch", "roll", "yaw", "error"]

        let xs = concatMap (get #x) traceMHs
        let ys = concatMap (get #y) traceMHs
        let pitches = concatMap (get #pitch) traceMHs
        let zs = concatMap (get #z) traceMHs
        let yaws = concatMap (get #yaw) traceMHs
        let rolls = concatMap (get #roll) traceMHs
        let errors = concatMap (get #error) traceMHs

        return (Scene {camera = Camera {x=head xs, y=head ys, pitch=head pitches, z=head zs, yaw=head yaws, roll=head rolls}}, 0.0)

        -- liftS $ print $ take 10 xs
        -- liftS $ print $ take 10 ys
        -- liftS $ print $ take 10 pitches
        -- liftS $ print $ take 10 zs
        -- liftS $ print $ take 10 yaws
        -- liftS $ print $ take 10 rolls

        -- liftS $ print =<< (testBed (head xs) (head ys) (head zs) (head pitches) (0.0) (head rolls))
        -- liftS $ print $ take 10 errors
        -- liftS $ print $ (length xs)
        -- liftS $ print $ ((fromIntegral (length xs)) / 100.0)


-- main = do
--     let imgHls = getHoughLines "data/road.jpg"
--     let err = compareLines imgHls [((0, 0), (0, 50)), ((20, 0), (20, 50))] (560, 315)
--     print imgHls
--     print err


data Args = Benchmark { outputPath :: String }
        | Run { inputPath :: String, outputPath :: String } deriving (Show, Data, Typeable)

inputPathMsg x = x &= help "Input path folder" &= opt "input"
outputPathMsg x = x &= help "Output path folder" &= opt "output"

benchmark = Benchmark { outputPath = outputPathMsg def }
run = Run { inputPath = inputPathMsg def, outputPath = outputPathMsg def }

runBenchmark :: String -> IO ()
runBenchmark output = do
    putStrLn "Benchmarking"
    putStrLn output

runRoadMarkings :: String -> String -> IO ()
runRoadMarkings input output  = do
    putStrLn "Running"
    print input
    files <- getImagesInPath input
    let filePaths = map (\x -> input ++ "/" ++ x) files

    parameters <- mapM trainModel filePaths
    let (params, errors) = unzip parameters

    let zipped = zip3 files params errors

    saveScenes output (zip files params)

    outputString <- serializeResults zipped

    writeFile (output ++ "results.csv") outputString



serializeResults :: [(String, Scene, Double)] -> IO String
serializeResults results = do
    let lines = map (\(name, Scene {camera = Camera { x, y, z, yaw, pitch, roll}}, error) -> name ++ "," ++ show x ++ "," ++ show y ++ "," ++ show z ++ "," ++ show pitch ++ "," ++ show yaw ++ "," ++ show roll ++ "," ++ show error ++ "\n") results
    let outStr = "name,x,y,z,pitch,yaw,roll,error\n" : lines

    return $ concat outStr



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
execute (Benchmark outputPath) = runBenchmark outputPath
execute (Run inputPath outputPath) = runRoadMarkings inputPath outputPath

mode :: Mode (CmdArgs Args)
mode = cmdArgsMode $ modes [benchmark,run] &= help "Build helper program" &= program "maker" &= summary "Maker v1.0\nMake it"

main :: IO ()
main = do
    args <- cmdArgsRun mode
    execute args

-- main :: IO Int32
-- main = testBedExample


-- Functions:
-- 1. Run RoadMarkings on a folder
-- 2. Run benchmarking on generated images
-- Command line arguments
-- 1. {executable} benchmark {output} # runs benchmarking on generated images and saves the output to the specified json file
-- 2. {executable} run {path} {output} # runs RoadMarkings on a folder of images and saves the output to the specified json file
-- 3. {executable} 

-- name,(params,)*,error,execution time (s)
-- "image1.png",(params,)*,3000,40