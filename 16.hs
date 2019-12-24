{-# LANGUAGE BangPatterns #-}

import System.Environment (getArgs)
import System.IO (readFile)
import Debug.Trace
import Data.Char (intToDigit, digitToInt)

base = [0,1,0,-1]

pattern :: Int -> [Int]
pattern n = drop 1 $ cycle $ concatMap (replicate n) base

dotp :: [Int] ->  [Int] -> Int
dotp xs ys = sum $ zipWith (*) xs ys

onesDigit :: Int -> Int
onesDigit x = (abs x) `rem` 10

fft1 :: [Int] -> [Int]
fft1 xs = [onesDigit $ dotp xs $ pattern i | i <- [1 .. (length xs)]]

fft :: [Int] -> [[Int]]
fft = iterate fft1

toInt :: [Int] -> Int
toInt xs = foldl (\n d-> 10*n + d) 0 xs

main = do
  -- print "Test patterns"
  -- -- print $ take 10 $ pattern 0
  -- print $ take 10 $ pattern 1
  -- print $ take 10 $ pattern 2
  -- print $ take 10 $ pattern 3

  -- print "Test dot products"
  -- print $ dotp [1..8] $ pattern 1
  -- print $ dotp [1..8] $ pattern 2
  -- print $ dotp [1..8] $ pattern 3

  
  -- let test = take 5 $ fft [1 .. 8]
  -- putStrLn $ unlines $ map (map intToDigit) test
  
  inputString <- readFile "16.input.txt"
  let input = map digitToInt $ head $ lines inputString
  -- print "Part 1"
  -- print $ map intToDigit $ take 8 $ (fft input)!!100
  print "Part 2"
  let input2 = concat $ replicate 10000 input
  let l2 = length input2
  print $ "input length " ++ (show l2)
  let offset = toInt $ take 7 input
  print offset
  print $ fromIntegral offset / fromIntegral l2 -- offset is 91%
  -- through the signal

  -- Since we are reading at the end of the signal, we can take
  -- advantage of the fact that the last digits are easy to
  -- compute. First, note that the ith pattern begins with i-1 0s
  -- followed by i 1s. Second, note that for i > 1/2 the signal
  -- length, the ith pattern is just 0s until the ith digit, then 1s
  -- all the way through. The dot product in computing the ith digit,
  -- where i > 1/2 the signal length, is thus the sum of the digits
  -- from i to the end. Third note that scanr (+) 0 [1 .. 5] =
  -- [1+2+3+4+5, 2+3+4+5, 3+4+5, 4+5, 5]. This is the function that we
  -- want.
  let fftend1 :: [Int] -> [Int]
      fftend1 xs = map onesDigit $ scanr (+) 0 xs
      fftend :: [Int] -> [[Int]]
      fftend = iterate fftend1
  print $ map intToDigit $ take 8 $ (fftend $ drop offset input2)!!100

  
  
-- pattern :: Int -> [Int]
-- pattern n = [i | i <- base, j <- [0 .. n]] ++ pattern n

-- mysum :: [Int] -> Int
-- mysum xs = foldl (+) 0 xs

-- dot :: [Int] -> [Int] -> Int
-- dot xs ys = (abs (mysum [x * y | (x,y) <- zip xs ys])) `mod` 10
-- -- dot xs ys = (abs $ sum zs) `mod` 10
-- --  where zs = -- traceShowId $
-- --          [x * y | (x,y) <- zip xs ys]

-- applyN :: Int -> (a->a) -> a -> a
-- applyN 0 _ x = x
-- applyN n f x = applyN (n-1) f $! traceShow n $! f x

-- phase :: [Int] -> [Int]
-- phase input = trace "Phase" $! [drop i input `dot` drop i (drop 1 $ pattern i) | i <- take n [0 .. ]]
--   where n = length input

-- phases :: Int -> [Int] -> [Int]
-- phases n input = trace "Done with phases" $! applyN n phase input

-- readInts :: String -> [Int]
-- readInts s = [read [c] | c <- s, '0' <= c, '9' >= c]

-- repeatN :: Int -> [a] -> [a]
-- repeatN n xs = concat $! [xs | i <- [1 .. n]]
-- -- repeatN 0 _ = []
-- -- repeatN n xs = xs ++ repeatN (n-1) xs

-- main = do
--   -- putStrLn $ show $ take 20 $ pattern 0
--   -- putStrLn $ show $ take 20 $ pattern 1
--   -- putStrLn $ show $ take 20 $ pattern 2
--   -- putStrLn $ show $ take 20 $ pattern 3
--   -- let input = map (\c -> read [c])  "12345678"
--   -- putStrLn $ show $ input
--   -- putStrLn $ show $ input `dot` (drop 1 $ pattern 0)
--   -- putStrLn $ show $ phase input
--   -- putStrLn $ show $ phases 1 input
--   -- putStrLn $ show $ phases 2 input
--   -- putStrLn $ show $ phases 3 input
--   -- putStrLn $ show $ phases 4 input
--   [inputFile] <- getArgs
--   inputString <- readFile inputFile
--   let input = readInts inputString
--   putStrLn "Part 1"
--   putStrLn $ concat $ map show $ take 8 $ phases 100 input
--   putStrLn "Part 2"
--   let input2 = repeatN 10000 input
--   putStrLn $ "input length: " ++ (show $ length input)
--   putStrLn $! "input2 length: " ++ (show $ length input2)
--   let offset :: Int
--       offset = read $! take 7 inputString
--   putStrLn $! "offset: " ++ (show offset)
      
--   let input2' = drop offset input2
--   putStrLn $! "input2' length: " ++ (show $! length input2')
--   let result = phases 100 $! input2'
--   let message = concat $! map show $! take 8 result
--   putStrLn message
