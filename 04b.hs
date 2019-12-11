import Data.List.Split (splitOn)

main = putStrLn $ show $ length ans
  where ans = [p | p <- [158126 .. 624574], let s = show p, adjacent s == True, increasing s == True]
        adjacent (x:y:z:xs) | x == y && y == z = adjacent $ dropWhile (== x) xs
        adjacent [] = False
        adjacent (x:[]) = False
        adjacent (x:y:xs) | x == y = True
        adjacent (x:y:xs) | otherwise = adjacent (y:xs)
        increasing [] = True
        increasing (x:[]) = True
        increasing (x:y:xs) | x > y = False
                            | otherwise = increasing (y:xs)
