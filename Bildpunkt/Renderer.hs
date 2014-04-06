{-# LANGUAGE ScopedTypeVariables #-}
module Bildpunkt.Renderer 
  (render)
where

import Data.Array.Accelerate as A
--import Data.Array.Accelerate.Interpreter as I
import Data.Array.Accelerate.CUDA as C
import Bildpunkt.Common

render :: Camera -> Resolution -> Int -> DistanceField -> Array DIM2 Color
render camera resolution numSteps f = colors
  where
    rays   = C.run $ marchAll camera resolution numSteps f
    colors = C.run $ toneMapAll f rays

toneMapAll :: DistanceField -> Array DIM2 Ray -> Acc (Array DIM2 Color)
toneMapAll f = A.map (toneMap f) . use

toneMap :: DistanceField -> Exp Ray -> Exp Color
toneMap f ray = ( (distanceToRay f ray) <=* (constant 0.01) )
              ? ( constant (255, 255, 255)
                , constant (0  , 0  , 0)
                )

marchAll :: Camera -> Resolution -> Int -> DistanceField -> Acc (Array DIM2 Ray)
marchAll camera resolution numSteps f = 
  A.map (A.iterate (constant numSteps) $ march f) $ genRays camera resolution

march :: DistanceField -> Exp Ray -> Exp Ray
march f ray = moveOrigin (distanceToRay f ray) ray

distanceToRay :: DistanceField -> Exp Ray -> Exp Float
distanceToRay f ray = f (A.fst ray)

genRays :: Camera -> Resolution -> Acc (Array DIM2 Ray)
genRays (cPos,cDir,cW,cH) (resW, resH) =
  generate (constant $ Z:.resH:.resW)
           (\ix -> let Z:.(y::Exp Int):.(x::Exp Int) = unlift ix
                   in
                     lift (cPos, rayDir x y))
  where
    cRight     = vecNormalize $ vecCross (constant cDir) $ constant (0.0, 1.0, 0.0)
    cUp        = vecNormalize $ vecCross cRight (constant cDir)
    cLowerLeft = vecAdd (vecScale cRight (constant $ cW * (-0.5)))
                        (vecScale cUp    (constant $ cH * (-0.5)))
    rayDir x y = vecNormalize $ vecAdd (constant cDir)
                              $ vecAdd cLowerLeft
                              $ vecAdd (vecScale cRight x')
                                       (vecScale cUp    y')
      where
        x' = (constant cW) * (((A.fromIntegral x) + 0.5) / (A.fromIntegral $ constant resW))
        y' = (constant cH) * (((A.fromIntegral y) + 0.5) / (A.fromIntegral $ constant resH))
