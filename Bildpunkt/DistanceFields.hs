module Bildpunkt.DistanceFields where

import Data.Array.Accelerate
import Bildpunkt.Common

sphere :: Exp Float -> DistanceField
sphere r p = vecLength p - r
