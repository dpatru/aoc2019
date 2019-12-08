w = 25
h = 6


-- drop n elements from the list until condition is met.
dropNUntil :: Int -> (a -> Bool) -> [a] -> [a]
dropNUntil _ _ [] = []
dropNUntil n f xs =
  if (f $ head xs)
  then xs
  else dropNUntil n f $ drop n xs
                  
main = interact $ unlines . draw w h . flatten (w*h) .  head . lines
  where flatten :: Int -> [Char] -> [Char] -- flatten images by selecting the first non-transparent pixel
        flatten size s = [(head $ dropNUntil size (/= '2') $ drop i s) | i <- [0 .. (size-1)]]
        draw rows columns pixels = [concatMap drawPixel $ take rows $ drop (i * rows) pixels | i <- [0 .. (columns - 1)]] 
        drawPixel x = if x == '1' then "# " else "  " -- looks better spaced out
        -- drawPixel x = if x == '1' then "#" else " " -- too cramped
