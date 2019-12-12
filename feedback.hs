import Debug.Trace

plus1 :: [Int]->[Int]
plus1 [] = []
plus1 (x:xs) = traceShowId (x+1): plus1 xs

to10 :: [Int] -> [Int]
to10 (x:xs)
  | x < 10 = x : to10 xs
  | otherwise = []


to10plus :: [Int] -> ([Int], Int)
to10plus (x:xs)
  | x < 10 = -- traceShowId $ 
      (x, 1) `merge` (to10plus xs)
  | otherwise = ([], 0)
  where merge (a, b) ~(as, bs) = (a:as, b+bs)
  -- note the irrefutable match operator ~, without this, to10plus
  -- causes a runtime error below. See
  -- https://stackoverflow.com/questions/59297557/when-can-i-rely-on-haskell-to-read-a-list-lazily/59298311#59298311.

main = do
  let out = plus1 $ 1: to10 out
  putStrLn $ show out

  let out = plus1 $ 1: out2
      out2 = to10 out
  putStrLn $ show out

  let out = plus1 $ 1: out2
      (out2, count) = to10plus out
  putStrLn $ show (out, count)
