module Bildpunkt.Config where

import Bildpunkt.Common

data Config = Config {
    camera        :: Camera
  , resolution    :: Resolution
  , numSteps      :: Int
  , distanceField :: DistanceField
  }