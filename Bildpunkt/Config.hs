module Bildpunkt.Config where

import Bildpunkt.Common

data Config = Config {
    epsilon             :: Float
  , camera              :: Camera
  , resolution          :: Resolution
  , numSteps            :: Int
  , distanceField       :: DistanceField
  , backgroundColor     :: Color
  , pointLight          :: PointLight
  , ambientLight        :: Color
  }
