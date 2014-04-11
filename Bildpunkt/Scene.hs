module Bildpunkt.Scene where

import Bildpunkt.DistanceField
import Bildpunkt.Common
import Bildpunkt.Config

simple1 = defaultConfig
  { camera        = ((10,3,-5), (0,1,0), 45)
  , pointLight    = ((3,3,2), (1.0, 1.0, 1.0))
  , ambientLight  = (0.3, 0.3, 0.3)
  , distanceField = unions 
      [ translate (0,1,0) $ sphere (1, 0.5, 0.5) 1
      , translate (1,1,2) $ box white (1,1,1)
      , translate (1,2.3,2) $ rotateY 20 $ box (0.5, 0.5, 1) (0.3,0.3,0.3)
      , plane white (0,1,0)
      ] 
  }
