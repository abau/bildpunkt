import           Data.Array.Accelerate (indexArray,Z(..),(:.)(..))
import           Codec.Picture (savePngImage,generateImage,DynamicImage(..),PixelRGB8(..))
import           Bildpunkt.DistanceField
import           Bildpunkt.Common
import           Bildpunkt.Renderer (render)
import           Bildpunkt.Config

main :: IO ()
main = savePngImage "test.png" $ ImageRGB8 $ generateImage writePixel resW resH
  where
    colors = render config

    writePixel x y = PixelRGB8 r g b
      where
        (r,g,b) = indexArray colors (Z :. (resH - y - 1) :. x)

    (resW, resH) = resolution config
    config       = Config { epsilon          = 0.001
                          , camera           = ((10,3,-5), (0,1,0), 45)
                          , resolution       = (800, 800)
                          , numSteps         = 1000
                          , distanceField    = scene
                          , backgroundColor  = black
                          , pointLight       = ((3,3,2), (1.0, 1.0, 1.0))
                          , ambientLight     = (0.3, 0.3, 0.3)
                          , shadowBlur       = 30
                          , numShadowSamples = 100
                          }

    scene = unions [ translate (0,1,0)  $ sphere (1, 0.5, 0.5) 1
                   , translate (1,1,2) $ box    (0.5, 0.5, 1) (1,1,1)
                   , plane white (0,1,0)
                   ]
