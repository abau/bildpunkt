{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.DistanceField where

import Prelude hiding ((++),fst)
import Data.Array.Accelerate hiding (Vector)
import Bildpunkt.Common

data DistanceField = DistanceField {
    evaluate      :: Exp Vector -> Exp Float
  , evaluateColor :: Exp Vector -> Exp (Float, Color)
  }

withColor :: Color -> (Exp Vector -> Exp Float) -> DistanceField
withColor color eval = DistanceField eval $ \v -> lift (eval v, constant color)

sphere :: Color -> Float -> DistanceField
sphere c r = withColor c $ \p -> vecLength p - (constant r)

box :: Color -> Vector -> DistanceField
box c m = withColor c $ \p -> vecLength $ vecMax (constant (0,0,0)) (vecSub (vecAbs p) 
                                                                            (constant m))

plane :: Color -> Direction -> DistanceField
plane c n = withColor c $ \p -> vecDot p $ vecNormalize $ constant n

union :: DistanceField -> DistanceField -> DistanceField
union f1 f2 = DistanceField 
  (\p -> min (evaluate f1 p) (evaluate f2 p))
  (\p -> let r1 = evaluateColor f1 p
             r2 = evaluateColor f2 p
         in
           cond ( (fst r1) <* (fst r2) ) r1 r2
  )

unions :: [DistanceField] -> DistanceField
unions = foldl1 union

intersection :: DistanceField -> DistanceField -> DistanceField
intersection f1 f2 = DistanceField 
  (\p -> max (evaluate f1 p) (evaluate f2 p))
  (\p -> let r1 = evaluateColor f1 p
             r2 = evaluateColor f2 p
         in
           cond ( (fst r1) >* (fst r2) ) r1 r2
  )

intersections :: [DistanceField] -> DistanceField
intersections = foldl1 intersection

subtract :: DistanceField -> DistanceField -> DistanceField
subtract f1 f2 = DistanceField 
  (\p -> max (evaluate f1 p) (- (evaluate f2 p)))
  (\p -> let r1      = evaluateColor f1 p
             (d2,c2) = unlift (evaluateColor f2 p) :: (Exp Float, Exp Color)
             invR2   = lift (-d2, c2)
         in
           cond ( (fst r1) >* (fst invR2) ) r1 invR2 
  )

move :: Position -> DistanceField -> DistanceField
move t = transform $ \p -> vecSub p $ constant t

transform :: (Exp Vector -> Exp Vector) -> DistanceField -> DistanceField
transform t f = DistanceField
  (\p -> evaluate      f $ t p)
  (\p -> evaluateColor f $ t p)
