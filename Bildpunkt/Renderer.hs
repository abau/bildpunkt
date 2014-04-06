{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.Renderer 
  (render)
where

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
toneMap config ray = 
        ( d <=* (constant $ backgroundThreshold config) )
              ? ( trueColor color
                , trueColor $ constant $ backgroundColor config
                )
  where
    (d, color)  = (unlift $ evaluate (distanceField config) ray) :: (Exp Float, Exp Color)
    trueColor c = lift (A.floor $ r * 255, A.floor $ g * 255, A.floor $ b * 255)
      where
        (r,g,b) = unlift c

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
    cLowerLeft        = vecAdd (vecScale cRight (constant $ cW * (-0.5)))
                               (vecScale cUp    (constant $ cH * (-0.5)))
    rayDir x y        = vecNormalize $ vecAdd (constant cDir)
                                     $ vecAdd cLowerLeft
                                     $ vecAdd (vecScale cRight x')
                                              (vecScale cUp    y')
      where
        x' = (constant cW) * (((A.fromIntegral x) + 0.5) / (A.fromIntegral $ constant resW))
        y' = (constant cH) * (((A.fromIntegral y) + 0.5) / (A.fromIntegral $ constant resH))
