import           Data.Array.Accelerate (indexArray,Z(..),(:.)(..),constant)
import           Codec.Picture (savePngImage,generateImage,DynamicImage(..),PixelRGB8(..))
import           Bildpunkt.DistanceFields
import           Bildpunkt.Renderer (render)

main :: IO ()
main = savePngImage "test.png" $ ImageRGB8 $ generateImage writePixel resW resH
  where
    colors = render camera resolution numSteps distanceField

    writePixel x y = PixelRGB8 (fromIntegral r) (fromIntegral g) (fromIntegral b)
      where
        (r,g,b) = indexArray colors (Z :. (resH - y - 1) :. x)

    camera                 = ((5,0,0), (-1,0,0), 2, 2)
    resolution@(resW,resH) = (200, 200)
    numSteps               = 10
    distanceField          = sphere $ constant 1
