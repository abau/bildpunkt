module Bildpunkt.Config where

import Bildpunkt.Common
import Bildpunkt.DistanceField (DistanceField)

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
