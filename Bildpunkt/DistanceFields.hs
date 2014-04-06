{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.DistanceFields where

import Prelude hiding ((++))
import Data.Array.Accelerate hiding (Vector)
import Bildpunkt.Common

sphere :: Float -> DistanceField
sphere r p = vecLength p - (constant r)

move :: Vector -> DistanceField -> DistanceField
move v f p = f $ vecSub p (constant v)

intersection :: DistanceField -> DistanceField -> DistanceField
intersection f1 f2 p = cond ( r1 <* r2 ) r2 r1
  where
    r1 = f1 p
    r2 = f2 p
