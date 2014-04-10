{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.Common where

import Data.Array.Accelerate hiding (Vector)

type Vector         = (Float, Float, Float)
type Position       = Vector
type Direction      = Vector
type Normal         = Direction
type Ray            = (Position, Direction)
type Camera         = (Position, Direction, Float, Float)
type Resolution     = (Int,Int)
type Color          = Vector
type PointLight     = (Position, Color)
type UnliftedVector = (Exp Float, Exp Float, Exp Float)

red,green,blue,black,white :: Color
red   = (1,0,0)
green = (0,1,0)
blue  = (0,0,1)
black = (0,0,0)
white = (1,1,1)

moveOrigin :: Exp Float -> Exp Ray -> Exp Ray
moveOrigin f = lift1 $ \(p :: Exp Position, d :: Exp Direction)
                     -> (vecAdd p (vecScale f d), d)

vecNormalize :: Exp Vector -> Exp Vector
vecNormalize = lift1 (vecNormalize' :: UnliftedVector -> UnliftedVector)

vecNormalize' :: Floating a => (a,a,a) -> (a,a,a)
vecNormalize' v = vecScale' (1 / (vecLength' v)) v

vecLength :: Exp Vector -> Exp Float
vecLength = lift1 (vecLength' :: UnliftedVector -> Exp Float)

vecLength' :: Floating a => (a,a,a) -> a
vecLength' = sqrt . vecLengthSqr'

vecLengthSqr :: Exp Vector -> Exp Float
vecLengthSqr = lift1 (vecLengthSqr' :: UnliftedVector -> Exp Float)

vecLengthSqr' :: Num a => (a,a,a) -> a
vecLengthSqr' v = vecDot' v v

vecAbs :: Exp Vector -> Exp Vector
vecAbs = lift1 (vecAbs' :: UnliftedVector -> UnliftedVector)

vecAbs' :: Num a => (a,a,a) -> (a,a,a)
vecAbs' (x, y, z) = (abs x, abs y, abs z)

vecMax :: Exp Vector -> Exp Vector -> Exp Vector
vecMax = lift2 (vecMax' :: UnliftedVector -> UnliftedVector -> UnliftedVector)

vecMax' :: Ord a => (a,a,a) -> (a,a,a) -> (a,a,a)
vecMax' (x, y, z) (u, v, w) = (max x u, max y v, max z w)

vecMin :: Exp Vector -> Exp Vector -> Exp Vector
vecMin = lift2 (vecMin' :: UnliftedVector -> UnliftedVector -> UnliftedVector)

vecMin' :: Ord a => (a,a,a) -> (a,a,a) -> (a,a,a)
vecMin' (x, y, z) (u, v, w) = (min x u, min y v, min z w)

vecCross :: Exp Vector -> Exp Vector -> Exp Vector
vecCross = lift2 (vecCross' :: UnliftedVector -> UnliftedVector -> UnliftedVector)

vecCross' :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
vecCross' (x, y, z) (u, v, w) = ((y*w) - (z*v), (z*u) - (x*w), (x*v) - (y*u))

vecDot :: Exp Vector -> Exp Vector -> Exp Float
vecDot = lift2 (vecDot' :: UnliftedVector -> UnliftedVector -> Exp Float)

vecDot' :: Num a => (a,a,a) -> (a,a,a) -> a
vecDot' (x, y, z) (u, v, w) = (x * u) + (y * v) + (z * w)

vecSub :: Exp Vector -> Exp Vector -> Exp Vector
vecSub = lift2 (vecSub' :: UnliftedVector -> UnliftedVector -> UnliftedVector)

vecSub' :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
vecSub' (x, y, z) (u, v, w) = (x - u, y - v, z - w)

vecScale :: Exp Float -> Exp Vector -> Exp Vector
vecScale f = lift1 (vecScale' f :: UnliftedVector -> UnliftedVector)

vecScale' :: Num a => a -> (a,a,a) -> (a,a,a)
vecScale' f (x, y, z) = (f * x, f * y, f * z)

vecTimes :: Exp Vector -> Exp Vector -> Exp Vector
vecTimes = lift2 (vecTimes' :: UnliftedVector -> UnliftedVector -> UnliftedVector)

vecTimes' :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
vecTimes' (x, y, z) (u, v, w) = (x * u, y * v, z * w)

vecAdd :: Exp Vector -> Exp Vector -> Exp Vector
vecAdd = lift2 (vecAdd' :: UnliftedVector -> UnliftedVector -> UnliftedVector)

vecAdd' :: Num a => (a,a,a) -> (a,a,a) -> (a,a,a)
vecAdd' (x, y, z) (u, v, w) = (x + u, y + v, z + w)

vecInvert :: Exp Vector -> Exp Vector
vecInvert = lift1 (vecInvert' :: UnliftedVector -> UnliftedVector)

vecInvert' :: Num a => (a,a,a) -> (a,a,a)
vecInvert' (x, y, z) = (-x,-y,-z)
