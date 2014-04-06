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
render config = colors
  where
    rays   = C.run $ marchAll   config
    colors = C.run $ toneMapAll config rays

toneMapAll :: Config -> Array DIM2 Ray -> Acc (Array DIM2 (Word8, Word8, Word8))
toneMapAll config = A.map (toneMap config) . use

toneMap :: Config -> Exp Ray -> Exp (Word8, Word8, Word8)
toneMap config ray = cond (d >* (constant $ backgroundThreshold config))
  (trueColor $ constant $ backgroundColor config)
  (trueColor $ shadePoint config (A.fst ray) color)
  where
    (d, color)  = (unlift $ evaluate (distanceField config) ray) :: (Exp Float, Exp Color)
    trueColor c = lift (trueComponent r, trueComponent g, trueComponent b)
      where
        (r,g,b) = unlift c
    
    trueComponent c = cond (c >* 1) (constant 255) (A.floor $ c * 255)

shadePoint :: Config -> Exp Position -> Exp Color -> Exp Color
shadePoint config point color = vecAdd ambient direct
  where
    ambient  = constant $ ambientLight config
    direct   = vecScale factor color
    factor   = vecDot normal $ vecNormalize $ vecSub lightPos point
    normal   = approxNormal config point
    lightPos = constant $ P.fst $ pointLight config

approxNormal :: Config -> Exp Position -> Exp Normal
approxNormal config point = vecNormalize $ lift (nX, nY, nZ)
  where
    nX   = (f (vecAdd point epsX)) - (f (vecSub point epsX))
    nY   = (f (vecAdd point epsY)) - (f (vecSub point epsY))
    nZ   = (f (vecAdd point epsZ)) - (f (vecSub point epsZ))
    f    = A.fst . distanceField config
    eps  = normalApproxEps config
    epsX = constant (eps, 0, 0)
    epsY = constant (0, eps, 0)
    epsZ = constant (0, 0, eps)

marchAll :: Config -> Acc (Array DIM2 Ray)
marchAll config = 
  A.map (A.iterate (constant $ numSteps config) $ march $ distanceField config) 
        (genRays config)

march :: DistanceField -> Exp Ray -> Exp Ray
march f ray = moveOrigin (A.fst $ evaluate f ray) ray

evaluate :: DistanceField -> Exp Ray -> Exp (Float, Color)
evaluate f ray = f $ A.fst ray

genRays :: Config -> Acc (Array DIM2 Ray)
genRays config =
  generate (constant $ Z:.resH:.resW)
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
