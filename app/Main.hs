{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}

module Main (main) where

import Prog ( call )
import Effects.ObsReader ( ObsReader(Ask) )
import Model ( Model(Model), bernoulli, uniform )
import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
import Effects.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Observables, Assign((:=)), (<:>), nil )
import Inference.SIM ( runSimulate )
import Sampler ( sampleIO )

import Data.Either

import Graphics.Image.IO
import Graphics.Image.Types
import Graphics.Image.Interface
import GHC.Real (Integral(toInteger))


coinFlip
  :: (Observables env '["p"] Double
    , Observables env '["y"] Bool)
  => Model env es Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y

makeImageGradient :: (Int, Int) -> Image VS RGB Word8
makeImageGradient dim = makeImage dim (\(x, y) -> PixelRGB  0 (fromIntegral ((x + y) `mod` 255)) 0 )

loadAndSave :: String -> String -> IO ()
loadAndSave source dest = do
  image <- readImageExact PNG source :: IO (Either String (Image VS RGBA Word8))
  case image of
    Left msg -> putStrLn msg
    Right image -> writeImageExact PNG [] dest image

drawFnToImage :: (Int, Int) -> ((Float, Float) -> Float) -> Float -> Image VS RGB Word8
drawFnToImage dim fn e = makeImage dim (\(y, x) -> PixelRGB (value x y) (value x y) (value x y) )
  where
    solve :: Float -> Word8
    solve a = if abs a <= e then 255 else 0
    value x y = solve (fn (fromIntegral x, fromIntegral y))

--- Probabilistic Model

data RoadSample = RoadSample 
    { centerX :: Double 
    , width :: Double
    }

--- Image processing

-- data Image

loadImage :: String -> IO (Image VS RGB Word8)
loadImage = readImageExact' PNG




renderImage :: RoadSample -> (Int, Int) -> Image VS RGB Word8
renderImage rs dim@(imWidth, imHeight) = drawFnToImage dim roadFn 0.0
  where
    roadFn (x, _) = if normalized x >= (realToFrac (centerX rs)) - halfRsWidth && normalized x <= (realToFrac (centerX rs)) + halfRsWidth then 0 else 1
    normalized x = x / fromIntegral imWidth :: Float
    halfRsWidth = (realToFrac (width rs)) / 2.0 :: Float


-- energyFunction :: RoadSample -> Image -> Double
-- energyFunction = undefined

-- main :: IO ()
-- main = do
--   frog <- readImageExact PNG "ProphandEspa.PNG" :: IO (Either String (Image VS RGBA Word8))
--   let img = makeImageGradient 255 255
--   writeImageExact PNG [] "wow.png" img
  -- case frog of
  --   Left msg -> putStrLn msg
  --   Right image -> writeImageExact PNG [] "wow.png" image

main :: IO ()
main = writeImageExact PNG [] "1.png" (renderImage (RoadSample {centerX = 0.8, width = 0.2}) (100, 100))