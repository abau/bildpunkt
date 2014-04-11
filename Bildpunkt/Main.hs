{-# LANGUAGE LambdaCase #-}
import           System.Environment (getArgs,getProgName)
import           Data.Array.Accelerate (indexArray,Z(..),(:.)(..))
import           Codec.Picture (savePngImage,generateImage,DynamicImage(..),PixelRGB8(..))
import           Bildpunkt.Renderer (render)
import           Bildpunkt.Config
import qualified Bildpunkt.Scene as Scene

main :: IO ()
main = getArgs >>= \case
  [sceneName, image] -> savePngImage image $ renderScene sceneName
  _                  -> do
    name <- getProgName
    error $ unwords ["Syntax:",name,"scene","image.png"]
  where
    renderScene sceneName = ImageRGB8 $ generateImage writePixel resW resH
      where
        colors         = render config
        writePixel x y = PixelRGB8 r g b
          where
            (r,g,b) = indexArray colors (Z :. (resH - y - 1) :. x)

        (resW, resH) = resolution config
        config       = case sceneName of
          "default" -> defaultConfig
          "simple1" -> Scene.simple1
          scene     -> error $ "unknown scene '" ++ scene ++ "'"
