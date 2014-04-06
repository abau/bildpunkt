{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.DistanceFields where

import Prelude hiding ((++))
import Data.Array.Accelerate hiding (Vector)
import Bildpunkt.Common

sphere :: Float -> DistanceField
sphere r x = vecLength x - (constant r)

plane :: Direction -> DistanceField
plane n x = vecDot x $ vecNormalize $ constant n

move :: Position -> DistanceField -> DistanceField
move p f x = f $ vecSub x (constant p)

union :: DistanceField -> DistanceField -> DistanceField
union f1 f2 x = cond ( r1 <* r2 ) r1 r2
  where
    r1 = f1 x
    r2 = f2 x

intersection :: DistanceField -> DistanceField -> DistanceField
intersection f1 f2 x = cond ( r1 <* r2 ) r2 r1
  where
    r1 = f1 x
    r2 = f2 x
