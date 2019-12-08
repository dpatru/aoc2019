import System.IO (readLn)
import Data.Word ( Word8 )
import Data.Vector.Storable ( fromList )
import Codec.Picture (DynamicImage(ImageY8), savePngImage)
import Codec.Picture
-- import Codec.Picture.Png.Internal.Export (ImageY8)

w = 25
h = 6

--main = interact $ unlines . showImg w . resolve (w*h-1) (w*h) .  head . lines
main = do
  chars <- getLine
  let img = ImageY8 . Image w h . fromList $ resolve (w*h-1) (w*h) chars
      resolve :: Int -> Int -> [Char] -> [Word8]
      resolve i size image
        | i < 0 =  []
        | otherwise = resolve' size image : resolve (i-1) size (tail image)
      resolve' :: Int -> [Char] -> Word8
      resolve' size (pixel: image) 
        | pixel == '2' = resolve' size $ drop size image
        | pixel == '0' = 0 -- 255
        | pixel == '1' = 255 -- 0
        | otherwise = error "bad value"
  savePngImage "8b.png" img
          
