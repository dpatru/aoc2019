
w = 25
h = 6

count :: Eq a => a -> [a] -> Int
count x xs = length $ filter (== x) xs

-- find the nonTransparent pixel by checking every w*h-th pixel
nonTransparent :: [Char] -> Char
nonTransparent xs | head xs /= '2' = head xs
                  | otherwise = nonTransparent $ drop (w*h) xs

draw :: [Char] -> [[Char]]
draw pixels = [concatMap drawPixel row
              | i <- [0 .. (h - 1)]
              , let row = take w $ drop (i * w) pixels] 
drawPixel x = if x == '1' then "# " else "  "

main = do
   pixels <- getLine
   -- Part 1
   let results = [(c '0', c '1' * c '2')
                 | i <- [0, w*h .. (length pixels - 1)]
                 , let layer = take (w*h) $ drop i pixels
                 , let c x = count x layer]
   putStrLn $ show $ minimum results 
   -- Part 2
   putStrLn $ unlines $ draw  [nonTransparent $ drop i pixels
                              | i <- [0 .. w*h-1]]

                
