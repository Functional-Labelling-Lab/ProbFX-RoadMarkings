{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedLabels #-}

module Benchmark (main) where

import Sampler (sampleIO)
import Main (initRoadSample, trainModel)
import Env ( Env, Observables, Assign((:=)), (<:>), nil, get )
import Inference.SIM (simulate)
import CppFFI (renderScene)
import Foreign (Storable(..))
import Foreign.Marshal (malloc)

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
  image <- do
    s <- malloc
    poke s scene
    renderScene s

  -- Use mh to work backwards to the origional scene
  -- TODO: trainModel needs to be altered to take all the above stuff as input,
  --       and return the predicted scene
  predictedScene <- trainModel

  -- TODO: replace undefined with some diff function which says how close
  --       to the input parameters we got.
  return (undefined scene predictedScene)


main :: IO ()
main = do
  benchmark


