import           Data.Array.Accelerate (indexArray,Z(..),(:.)(..))
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

    camera                 = ((5,1,0), (-1,0,0), 2, 2)
    resolution@(resW,resH) = (600, 600)
    numSteps               = 100
    distanceField          = union (move (0, 0.5,0) $ sphere 1)
                           $ plane (0,1,0)
