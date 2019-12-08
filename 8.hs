
w = 25
h = 6

count :: Eq a => a -> [a] -> Int
count x [] = 0
count x (y:ys) | x == y = 1 + count x ys
               | otherwise = count x ys
               
search :: Int -> [(Int, Int)] -> [Char] -> [(Int, Int)]
search size ((result, best):xs) [] = ((result, best):xs)
search size ((result, best):xs) layers =
  search size (if best < c0 then ((result, best):(c1*c2, c0):xs) else ((c1 * c2, c0):(result,best):xs)) layersRest
  where (layer, layersRest) = splitAt size layers
        c0 = count '0' layer
        c1 = count '1' layer
        c2 = count '2' layer
        
--main = interact $ show . length
--main = interact $ show . splitAt (w*h) 
main = interact $ unlines . map (show . search (w*h) [(0,w*h+1)]) . lines
-- main = interact $ show . calculate . select . layer
  -- where calculate :: [Char] -> Int
  --       calculate img = ones * twos
  --         where ones = count '1' img
  --               twos = count '2' img
  --       select :: ([Char],[Char]) -> [Char]
  --       select (a,b) = if za < zb then a else b
  --         where za = count '0' a
  --               zb = count '0' b
  --       layer :: [Char] -> ([Char],[Char])
  --       layer = splitAt (w*h)
        
                
