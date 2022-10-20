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
import Graphics.Image (toLists)
import Graphics.Image.IO ( readImageExact', writeImageExact )
import Graphics.Image.Types
import Graphics.Image.Interface ( Image, dims, makeImage )
import GHC.Real (Integral(toInteger))

--- Probabilistic Model

data RoadSample = RoadSample 
    { centerX :: Double 
    , width :: Double
    }

--- Image processing

type ImageData = Image VS RGB Word8

drawFnToImage :: (Int, Int) -> ((Float, Float) -> Float) -> Float -> ImageData
drawFnToImage dim fn e = makeImage dim (\(y, x) -> PixelRGB (value x y) (value x y) (value x y) )
  where
    solve :: Float -> Word8
    solve a = if abs a <= e then 255 else 0
    value x y = solve (fn (fromIntegral x, fromIntegral y))

loadImage :: String -> IO ImageData
loadImage = readImageExact' PNG

renderImage :: RoadSample -> (Int, Int) -> ImageData
renderImage rs dim@(imWidth, imHeight) = drawFnToImage dim roadFn 0.0
  where
    roadFn (x, _) = if normalized x >= realToFrac (centerX rs) - halfRsWidth && normalized x <= realToFrac (centerX rs) + halfRsWidth then 0 else 1
    normalized x = x / fromIntegral imWidth :: Float
    halfRsWidth = realToFrac (width rs) / 2.0 :: Float

errorFunction :: RoadSample -> ImageData -> Double
errorFunction rs target = (avgDistFromBlack / fromIntegral (width + height)) + (avgDistFromWhite / fromIntegral (width + height))
  where
    im_dims@(width, height) = dims target
    drawnImage = renderImage rs im_dims 
    (maskedPixels, antiMaskedPixels) = partition mask zippedImages

    avgDistFromBlack = sum (Prelude.map (\(_, PixelRGB r g b) -> sqrt (fromIntegral r ^ 2 + fromIntegral g ^ 2 + fromIntegral b ^ 2) ) maskedPixels)

    avgDistFromWhite = sum (Prelude.map (\(_, PixelRGB r g b) -> sqrt ((fromIntegral r - 255) ^ 2 + (fromIntegral g - 255) ^ 2 + (fromIntegral b - 255) ^ 2) ) antiMaskedPixels)

    zippedImages :: [(Pixel RGB Word8, Pixel RGB Word8)]
    zippedImages = concat $ Prelude.zipWith zip (toLists drawnImage) (toLists target)

    mask :: (Pixel RGB Word8, Pixel RGB Word8) -> Bool
    mask (PixelRGB 255 255 255, _) = True
    mask (_, _) = False

--- Training

initRoadSample :: (Observables env ["centerX", "width"] Double) => Model env es RoadSample
initRoadSample = do
    centerX <- uniform 0 1 #centerX
    width <- uniform 0 1 #width
    return $ RoadSample centerX width

roadGenerationModel :: (Observables env ["centerX", "width", "error"] Double) => ImageData -> () -> Model env es ()
roadGenerationModel image _ = do
    roadSample <- initRoadSample
    error <- normal (errorFunction roadSample image) 300 #error
    return ()   

--- Main code

main :: IO Int
main = sampleIO $ do
    image <- liftS $ loadImage "data/example2.png"

    liftS $ print $ errorFunction (RoadSample 1 1) image

    let mh_env = (#centerX := []) <:> (#width := []) <:> (#error := repeat 0) <:> nil
    traceMHs <- mh 50000 (roadGenerationModel image) ((), mh_env) ["centerX", "width"]
    
    let traceMH = head traceMHs
    let centerX = head $ get #centerX traceMH
        width = head $ get #width traceMH
    liftS $ print (centerX, width, length traceMHs)

    let exampleImage = renderImage (RoadSample centerX width) (100, 100)
    liftS $ writeImageExact PNG [] "output2.png" exampleImage
    return 0