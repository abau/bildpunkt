{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.Renderer 
  (render)
where

import Prelude as P
import Data.Array.Accelerate as A
--import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.CUDA as C
import Bildpunkt.Common
import Bildpunkt.Config

render :: Config -> Array DIM2 (Word8, Word8, Word8)
render config = C.run $ let 
  rays          = marchAll config
  shadowSamples = evaluateShadowSamples config rays
  shadings      = A.zipWith (shade config) rays shadowSamples
  in
    A.map toneMap shadings

marchAll :: Config -> Acc (Array DIM2 Ray)
marchAll config = 
  A.map (A.iterate (constant $ numSteps config) $ march) 
        (genRays config)

  where
    march ray = moveOrigin (A.fst $ evaluate config ray) ray

evaluateShadowSamples :: Config -> Acc (Array DIM2 Ray) -> Acc (Array DIM2 Float)
evaluateShadowSamples config rays = 
    A.fold1 min2
  $ A.zipWith eval (genShadowSamples config)
                   (A.replicate (lift $ Z:.All:.All:.n) rays)
  where
    eval t ray = cond (d <* (constant $ epsilon config) )
                      (constant 0)
                      (min2 (constant 1) blur)
      where 
        d        = A.fst $ distanceField config
                         $ vecAdd point (vecScale t lightDir)
        lightDir = vecSub (constant $ P.fst $ pointLight config) point
        lightD   = vecLength lightDir
        point    = A.fst ray
        blur     = ((constant $ shadowBlur config) * d) / (t * lightD)

    n = constant $ numShadowSamples config

shade :: Config -> Exp Ray -> Exp Float -> Exp Color
shade config ray shadowFactor = cond (d >* (constant $ epsilon config))
  (constant $ backgroundColor config)
  (vecAdd (vecTimes ambient color)
          (vecTimes direct  color)
  )
  where
    point        = A.fst ray
    (d,color)    = (unlift $ evaluate config ray) :: (Exp Float, Exp Color)
    ambient      = constant $ ambientLight config
    direct       = vecScale (directFactor * shadowFactor) directColor
    directFactor = vecDot normal $ vecNormalize $ vecSub directPos point
    normal       = approxNormal config point
    directPos    = constant $ P.fst $ pointLight config
    directColor  = constant $ P.snd $ pointLight config

toneMap :: Exp Color -> Exp (Word8, Word8, Word8)
toneMap color = lift (trueComponent r, trueComponent g, trueComponent b)
  where
    (r,g,b)         = (unlift color) :: (Exp Float, Exp Float, Exp Float)
    trueComponent c = cond (c >* 1) (constant 255) (A.floor $ c * 255)

genShadowSamples :: Config -> Acc (Array DIM3 Float)
genShadowSamples config = 
  generate (lift $ resolutionShape config :. n)
           (\ix -> let Z:.(_::Exp Int):.(_::Exp Int):.(i::Exp Int) = unlift ix
                   in
                     sample i
           )
  where
    n        = constant $ numShadowSamples config
    sample i = (A.fromIntegral $ i + 1) / (A.fromIntegral $ n + 1)

approxNormal :: Config -> Exp Position -> Exp Normal
approxNormal config point = vecNormalize $ lift (nX, nY, nZ)
  where
    nX   = (f (vecAdd point epsX)) - (f (vecSub point epsX))
    nY   = (f (vecAdd point epsY)) - (f (vecSub point epsY))
    nZ   = (f (vecAdd point epsZ)) - (f (vecSub point epsZ))
    f    = A.fst . distanceField config
    eps  = epsilon config
    epsX = constant (eps, 0, 0)
    epsY = constant (0, eps, 0)
    epsZ = constant (0, 0, eps)

genRays :: Config -> Acc (Array DIM2 Ray)
genRays config =
  generate (resolutionShape config)
           (\ix -> let Z:.(y::Exp Int):.(x::Exp Int) = unlift ix
                   in
                     lift (cPos, rayDir x y))
  where
    (cPos,cDir,cW,cH) = camera config
    (resW, resH)      = resolution config
    cRight            = vecNormalize $ vecCross (constant cDir) $ constant (0.0, 1.0, 0.0)
    cUp               = vecNormalize $ vecCross cRight (constant cDir)
    cLowerLeft        = vecAdd (vecScale (constant $ cW * (-0.5)) cRight)
                               (vecScale (constant $ cH * (-0.5)) cUp)
    rayDir x y        = vecNormalize $ vecAdd (constant cDir)
                                     $ vecAdd cLowerLeft
                                     $ vecAdd (vecScale x' cRight)
                                              (vecScale y' cUp   )
      where
        x' = (constant cW) * (((A.fromIntegral x) + 0.5) / (A.fromIntegral $ constant resW))
        y' = (constant cH) * (((A.fromIntegral y) + 0.5) / (A.fromIntegral $ constant resH))

evaluate :: Config -> Exp Ray -> Exp (Float, Color)
evaluate config ray = distanceField config $ A.fst ray

resolutionShape :: Config -> Exp DIM2
resolutionShape config = index2 (constant resH) (constant resW)
  where
    (resW, resH) = resolution config
