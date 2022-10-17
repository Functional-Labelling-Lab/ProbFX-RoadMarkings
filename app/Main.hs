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

import Lib

coinFlip
  :: (Observables env '["p"] Double
    , Observables env '["y"] Bool)
  => Model env es Bool
coinFlip = do
  p <- uniform 0 1 #p
  y <- bernoulli p #y
  return y

main :: IO ()
main = do
  let env = #p := [] <:> #y := [] <:> nil
  (b, _) <- sampleIO $ runSimulate env coinFlip
  putStrLn $ show $ b
