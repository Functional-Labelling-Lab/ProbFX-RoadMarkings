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
import MyLib
import Foreign (Storable(..), StablePtr(..))
import Foreign
import Foreign.Marshal.Alloc
import Effects.Lift (Lift)
import OpenSum (Member)
import System.IO.Unsafe
--- Probabilistic Model

-- data RoadSample = RoadSample 
--     { centerX :: Double 
--     , width :: Double
--     }

--- Image processing



--- Training

initRoadSample :: (Observables env ["x", "y", "z", "pitch", "yaw", "roll", "roadWidth"] Double) => Model env es Scene
initRoadSample = do
    x <- uniform (-1) 1 #x
    y <- uniform (-1) 1 #y
    z <- uniform (-1) 1 #z
    pitch <- uniform (-1) 1 #pitch
    yaw <- uniform (-1) 1 #yaw
    roll <- uniform (-1) 1 #roll
    roadWidth <- uniform (-1) 1 #roadWidth
    return $ Scene { camera = Camera {x=d x, y=d y, pitch=d pitch, z=d pitch, yaw=d yaw, roll=d roll}, roadWidth=d roadWidth }
      where
        d :: Double -> Float
        d = realToFrac

roadGenerationModel :: (Observables env ["x", "y", "z", "pitch", "yaw", "roll", "roadWidth", "error"] Double) => () -> Model env es ()
roadGenerationModel _ = do
    roadSample <- initRoadSample
    error <- normal (errorFunction roadSample) 0.2 #error
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



-- main :: IO ()
-- main = do
--   let s = Scene { camera = Camera {x=0.0, y=0.5, pitch=0.0, z=0.0, yaw=0.0, roll=0.0}, roadWidth=0.6 }
--   scene <- malloc
--   poke scene s

--   string <- newCString "/src/textures/no_road.jpg"
--   setTargetImg string
  
--   renderScene scene
--   findTextureDifference
--   diff <- getMeanPixelValue
--   print diff


--   print diff

main :: IO () 
main = print =<< testBed 0 0.5 0 0 0 0 0.6

-- main :: IO Int
-- main = do

--     string <- newCString "/src/textures/no_road.jpg"
--     setTargetImg string
    
--     sampleIO $ do
--         -- image <- liftS $ loadImage "data/example2.png"



--         liftS $ print $ errorFunction Scene { camera = Camera {x=0.0, y=0.5, pitch=0.0, z=0.0, yaw=0.0, roll=0.0}, roadWidth=0.6 }

--         -- let mh_env :: '["centerX" ':= ]
--         let mh_env = (#x := []) <:> (#y := []) <:> (#z := []) <:> (#pitch := []) <:> (#yaw := []) <:> (#roll := []) <:> (#roadWidth := []) <:> (#error := repeat 0) <:> nil
--         traceMHs <- mh 500 roadGenerationModel ((), mh_env) ["x", "y", "z", "pitch", "yaw", "roll", "roadWidth"]
        
--         let traceMH = head traceMHs
--         let x = head $ get #x traceMH
--             y = head $ get #y traceMH
--             z = head $ get #z traceMH
--             pitch = head $ get #pitch traceMH
--             yaw = head $ get #yaw traceMH
--             roll = head $ get #roll traceMH
--             roadWidth = head $ get #roadWidth traceMH
--         liftS $ print (x, y, z, pitch, yaw, roll, roadWidth, length traceMHs)

--         -- let exampleImage = renderImage (RoadSample centerX width) (100, 100)
--         -- liftS $ writeImageExact PNG [] "output2.png" exampleImage
--         return 0