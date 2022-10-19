--- Language Extensions & Imports
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

import Prog ( call )
import Effects.ObsReader ( ObsReader(Ask) )
import Model ( Model(Model), normal' )
import PrimDist ( PrimDist(BernoulliDist, UniformDist) )
import Effects.Dist ( Dist(Dist) )
import Data.Kind (Constraint)
import Env ( Observables, Assign((:=)), (<:>), nil )
import Inference.SIM ( runSimulate )
import Sampler ( sampleIO )

--- Probabilistic Model

data RoadSample = RoadSample 
    { centerX :: Double 
    , width :: Double
    }

--- Image processing

data Image

loadImage :: String -> Image
loadImage = undefined


renderImage :: RoadSample -> Image
renderImage = undefined

energyFunction :: Image -> Image -> Double
energyFunction = undefined


--- Training

initDistributionParams :: Model env es RoadDistributionParams
initDistributionParams = undefined

--- Main code  
