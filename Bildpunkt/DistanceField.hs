{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.DistanceField where

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

subtract :: DistanceField -> DistanceField -> DistanceField
subtract f1 f2 x = cond ( (fst r1) <* (-d2) ) invR2 r1
  where
    r1      = f1 x
    (d2,c2) = unlift (f2 x) :: (Exp Float, Exp Color)
    invR2   = lift (-d2, c2)
