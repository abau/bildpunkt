module Bildpunkt.Config where

import Bildpunkt.Common
import Bildpunkt.DistanceField (DistanceField,sphere)

data Config = Config {
    epsilon             :: Float
  , camera              :: Camera
  , resolution          :: Resolution
  , numSteps            :: Int
  , distanceField       :: DistanceField
  , backgroundColor     :: Color
  , pointLight          :: PointLight
  , ambientLight        :: Color
  , shadowBlur          :: Float
  , numShadowSamples    :: Int
  , numAOSamples        :: Int
  , aoDistance          :: Float
  , aoIntensity         :: Float
  }

defaultConfig :: Config
defaultConfig = Config { 
    epsilon          = 0.001
  , camera           = ((10,0,0), (0,0,0), 45)
  , resolution       = (800, 800)
  , numSteps         = 1000
  , distanceField    = sphere white 1
  , backgroundColor  = black
  , pointLight       = ((10,10,10), (1.0, 1.0, 1.0))
  , ambientLight     = (0, 0, 0)
  , shadowBlur       = 30
  , numShadowSamples = 100
  , numAOSamples     = 10
  , aoDistance       = 0.1
  , aoIntensity      = 1
  }
