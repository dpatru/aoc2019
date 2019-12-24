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
onesDigit x = (abs x) `mod` 10 -- mod returns the remainder from the
                               -- previous number, we want the
                               -- remainder from zero. So take the abs
                               -- before applying mod.

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

