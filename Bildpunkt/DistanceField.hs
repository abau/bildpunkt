{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.DistanceField where

import Prelude hiding ((++),fst)
import Data.Array.Accelerate hiding (Vector)
import Bildpunkt.Common

data DistanceField = DistanceField {
    evaluate      :: Exp Vector -> Exp Float
  , evaluateColor :: Exp Vector -> Exp (Float, Color)
  }

sphere :: Color -> Float -> DistanceField
sphere c r = withColor c $ \p -> vecLength p - (constant r)

box :: Color -> Vector -> DistanceField
box c m = withColor c $ \p -> vecLength $ vecMax (constant (0,0,0)) (vecSub (vecAbs p) 
                                                                            (constant m))

cylinder :: Color -> Float -> Float -> DistanceField
cylinder c h r = withColor c $ \p -> 
  let (x,y,z) = (unlift p) :: UnliftedVector
  in
    max ( vecLength (lift (x, constant 0, z)) - (constant r) )
        ( abs y - (constant h / 2) )

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

subtraction :: DistanceField -> DistanceField -> DistanceField
subtraction f1 f2 = DistanceField 
  (\p -> max (evaluate f1 p) (- (evaluate f2 p)))
  (\p -> let r1      = evaluateColor f1 p
             (d2,c2) = unlift (evaluateColor f2 p) :: (Exp Float, Exp Color)
             invR2   = lift (-d2, c2)
         in
           cond ( (fst r1) >* (fst invR2) ) r1 invR2 
  )

subtractions = foldl1 subtraction

translate :: Position -> DistanceField -> DistanceField
translate t = transform $ \p -> vecSub p $ constant t

rotateX :: Float -> DistanceField -> DistanceField
rotateX a = transform $ \p -> 
  let sinA = sin $ toRadian' (-a)
      cosA = cos $ toRadian' (-a)
  in
    lift $ ( (vecDot p $ constant (1, 0, 0))
           , (vecDot p $ constant (0, cosA, -sinA))
           , (vecDot p $ constant (0, sinA, cosA)) 
           )

rotateY :: Float -> DistanceField -> DistanceField
rotateY a = transform $ \p -> 
  let sinA = sin $ toRadian' (-a)
      cosA = cos $ toRadian' (-a)
  in
    lift $ ( (vecDot p $ constant (cosA, 0, sinA))
           , (vecDot p $ constant (0,1,0))
           , (vecDot p $ constant (-sinA, 0, cosA)) 
           )

rotateZ :: Float -> DistanceField -> DistanceField
rotateZ a = transform $ \p -> 
  let sinA = sin $ toRadian' (-a)
      cosA = cos $ toRadian' (-a)
  in
    lift $ ( (vecDot p $ constant (cosA, -sinA, 0))
           , (vecDot p $ constant (sinA,  cosA, 0))
           , (vecDot p $ constant (0, 0, 1)) 
           )

transform :: (Exp Vector -> Exp Vector) -> DistanceField -> DistanceField
transform t f = DistanceField
  (\p -> evaluate      f $ t p)
  (\p -> evaluateColor f $ t p)

withColor :: Color -> (Exp Vector -> Exp Float) -> DistanceField
withColor color eval = DistanceField eval $ \v -> lift (eval v, constant color)
