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
import Bildpunkt.DistanceField (DistanceField (..))

render :: Config -> Array DIM2 (Word8, Word8, Word8)
render config = C.run $ let 
  rays          = marchAll config
  shadowSamples = evaluateShadowSamples config rays
  aoSamples     = evaluateAOSamples config rays
  shadings      = A.zipWith3 (shade config) rays shadowSamples aoSamples
  in
    A.map toneMap shadings

marchAll :: Config -> Acc (Array DIM2 Ray)
marchAll config = A.map (A.iterate (constant $ numSteps config) $ march) genRays
  where
    march ray = moveOrigin (evaluateRay config ray) ray

    genRays =
      generate (resolutionShape config)
               (\ix -> let Z:.(y::Exp Int):.(x::Exp Int) = unlift ix
                       in
                         lift (cPos, rayDir x y))
      where
        (cPos,cLookAt,fov) = camera config
        (iW,iH)            = resolution config
        fovRad             = toRadian' fov
        cW                 = 2 * tan (fovRad / 2)
        cH                 = cW * (P.fromIntegral iH) / (P.fromIntegral iW)
        cDir               = vecNormalize' $ vecSub' cLookAt cPos 
        (resW, resH)       = resolution config
        cRight             = vecNormalize' $ vecCross' cDir (0.0, 1.0, 0.0)
        cUp                = vecNormalize' $ vecCross' cRight cDir
        cLowerLeft         = vecAdd' (vecScale' (cW * (-0.5)) cRight)
                                     (vecScale' (cH * (-0.5)) cUp)
        rayDir x y         = vecNormalize $ vecAdd (constant cDir)
                                          $ vecAdd (constant cLowerLeft)
                                          $ vecAdd (vecScale x' $ constant cRight)
                                                   (vecScale y' $ constant cUp   )
          where
            x' = (constant cW) * (((A.fromIntegral x) + 0.5) / (constant $ P.fromIntegral resW))
            y' = (constant cH) * (((A.fromIntegral y) + 0.5) / (constant $ P.fromIntegral resH))

evaluateShadowSamples :: Config -> Acc (Array DIM2 Ray) -> Acc (Array DIM2 Float)
evaluateShadowSamples config rays = 
    A.fold1 min
  $ A.zipWith eval genShadowSamples 
  $ A.replicate (lift $ Z:.All:.All:.n) rays
  where
    n = constant $ numShadowSamples config

    eval t ray = cond (d <* (constant $ epsilon config) )
                      (constant 0)
                      (min (constant 1) blur)
      where 
        d        = evaluate (distanceField config) (vecAdd point $ vecScale t lightDir)
        lightDir = vecSub (constant $ P.fst $ pointLight config) point
        lightD   = vecLength lightDir
        point    = A.fst ray
        blur     = ((constant $ shadowBlur config) * d) / (t * lightD)

    genShadowSamples = 
      generate (lift $ resolutionShape config :. n)
               (\ix -> let Z:.(_::Exp Int):.(_::Exp Int):.(i::Exp Int) = unlift ix
                       in
                         genSample i
               )

    genSample i = (A.fromIntegral $ i + 1) / (A.fromIntegral $ n + 1)

evaluateAOSamples :: Config -> Acc (Array DIM2 Ray) -> Acc (Array DIM2 Float)
evaluateAOSamples config rays =
    A.map (\s -> 1 - (constant (aoIntensity config) * s))
  $ A.fold1 (+)
  $ A.zipWith eval genAOSamples 
  $ A.replicate (lift $ Z:.All:.All:.n) rays
  where
    n          = constant $ numAOSamples config
    delta      = constant $ aoDistance config
    eval i ray = (t - d) / (2 ** i')
      where 
        i'     = (A.fromIntegral i) :: Exp Float
        normal = approxNormal config point
        d      = evaluate (distanceField config) (vecAdd point $ vecScale t normal)
        point  = A.fst ray
        t      = (A.fromIntegral $ i + 1) * delta
    
    genAOSamples =
      generate (lift $ resolutionShape config :. n)
               (\ix -> let Z:.(_::Exp Int):.(_::Exp Int):.(i::Exp Int) = unlift ix
                       in
                         i
               )

shade :: Config -> Exp Ray -> Exp Float -> Exp Float -> Exp Color
shade config ray shadowFactor aoFactor = cond (d >* (constant $ epsilon config))
  (constant $ backgroundColor config)
  (vecAdd (vecTimes ambient color)
          (vecTimes direct  color)
  )
  where
    point        = A.fst ray
    (d,color)    = (unlift $ evaluateColorRay config ray) :: (Exp Float, Exp Color)
    ambient      = vecScale aoFactor $ constant $ ambientLight config
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

approxNormal :: Config -> Exp Position -> Exp Normal
approxNormal config point = vecNormalize $ lift (nX, nY, nZ)
  where
    nX   = (f (vecAdd point epsX)) - (f (vecSub point epsX))
    nY   = (f (vecAdd point epsY)) - (f (vecSub point epsY))
    nZ   = (f (vecAdd point epsZ)) - (f (vecSub point epsZ))
    f    = evaluate $ distanceField config
    eps  = epsilon config
    epsX = constant (eps, 0, 0)
    epsY = constant (0, eps, 0)
    epsZ = constant (0, 0, eps)

evaluateRay :: Config -> Exp Ray -> Exp Float
evaluateRay config ray = evaluate (distanceField config) $ A.fst ray

evaluateColorRay :: Config -> Exp Ray -> Exp (Float, Color)
evaluateColorRay config ray = evaluateColor (distanceField config) $ A.fst ray

resolutionShape :: Config -> Exp DIM2
resolutionShape config = index2 (constant resH) (constant resW)
  where
    (resW, resH) = resolution config
