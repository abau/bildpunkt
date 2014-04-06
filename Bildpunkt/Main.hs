import           Data.Array.Accelerate (indexArray,Z(..),(:.)(..))
import           Codec.Picture (savePngImage,generateImage,DynamicImage(..),PixelRGB8(..))
import           Bildpunkt.DistanceFields
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
    config       = Config { epsilon         = 0.001
                          , camera          = ((5,1,0), (-1,0,0), 2, 2)
                          , resolution      = (600, 600)
                          , numSteps        = 1000
                          , distanceField   = union ( move   (0,1,0) 
                                                    $ sphere (1, 0.5, 0.5) 1
                                                    )
                                            $ plane white (0,1,0)
                          , backgroundColor = black
                          , pointLight      = ((3,3,2), (0.8, 0.7, 0.7))
                          , ambientLight    = (0.3, 0.3, 0.3)
                          }
