{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.DistanceFields where

import Prelude hiding ((++),fst)
import Data.Array.Accelerate hiding (Vector)
import Bildpunkt.Common

sphere :: Color -> Float -> DistanceField
sphere color r x = lift (vecLength x - (constant r), constant color)

plane :: Color -> Direction -> DistanceField
plane color n x = lift (vecDot x $ vecNormalize $ constant n, constant color)

move :: Position -> DistanceField -> DistanceField
move p f x = f $ vecSub x (constant p)

union :: DistanceField -> DistanceField -> DistanceField
union f1 f2 x = cond ( (fst r1) <* (fst r2) ) r1 r2
  where
    r1 = f1 x
    r2 = f2 x

intersection :: DistanceField -> DistanceField -> DistanceField
intersection f1 f2 x = cond ( (fst r1) <* (fst r2) ) r2 r1
  where
    r1 = f1 x
    r2 = f2 x
