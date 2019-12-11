#!/usr/bin/env stack
-- stack --resolver lts-14.16 script
{-# LANGUAGE BangPatterns #-}

import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Map.Strict (Map, (!), insert, elems, fromList, toList, findWithDefault, size, empty, member, findMin, findMax, singleton)
import qualified Data.Map.Strict as M
--import qualified Data.Array as A
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Complex (Complex((:+)), realPart, imagPart)

run :: (Integer, Integer) -> Map Integer Integer -> [Integer] -> [Integer]-- (instructionPointer, base), instructions, input, output
-- run i instructions inputs = unlines inputs
run (i, base) instructions inputs =
  -- trace ((show $ toList instructions) ++ ": (ip, base, instr, inputs) " ++ show (i, base, instr, inputs) ++ "\n") $
  case instr `mod` 100 of
    1 -> run (i+4, base) (insert (addr 3) (arg 1 + arg 2) instructions) inputs
    2 -> run (i+4, base) (insert (addr 3) (arg 1 * arg 2) instructions) inputs
    3 -> run (i+2, base) (insert (addr 1) (head inputs) instructions) $ tail inputs
    4 -> arg 1: run (i+2, base) instructions inputs
    5 -> run (if arg 1 == 0 then i+3 else arg 2, base) instructions inputs
    6 -> run (if arg 1 == 0 then arg 2 else i+3, base) instructions inputs
    7 -> run (i+4, base) (insert (addr 3) (if arg 1 < arg 2 then 1 else 0) instructions) inputs
    8 -> run (i+4, base) (insert (addr 3) (if arg 1 == arg 2 then 1 else 0) instructions) inputs
    9 -> run (i+2, base+arg 1) instructions inputs
    99 -> []
    _ -> error "unknown opcode"
  where instr = instructions!i
        ii x = findWithDefault 0 x instructions
        arg :: Integer -> Integer
        arg n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ ii $ i+n
          1 -> ii $ i+n
          2 -> ii ((ii $ i+n) + base)
          _ -> error "bad argument mode"
        addr n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ i+n
          1 -> error "address in mode 1"
          2 -> --trace "address in mode 2" $
            ii (i+n) + base
          _ -> error $ "address in unknown mode"

instance Ord a => Ord (Complex a) where
  compare a b | ar < br || ar == br && ai < bi = LT
              | ar > br || ar == br && ai > bi = GT
              | otherwise = EQ
    where ar = realPart a
          ai = imagPart a
          br = realPart b
          bi = imagPart b

toParts :: Complex a -> (a, a)
toParts c = (realPart c, imagPart c)

toIntParts :: RealFrac a => Complex a -> (Int, Int)
toIntParts c = (round $ realPart c, round $ imagPart c)

fromParts :: (a, a) -> Complex a
fromParts (a,b) = a :+ b

process :: (Complex Float, Complex Float, Map (Complex Float) Integer) -> [Integer] -> [Integer]
process (p, d, colors) [] =
  traceShow (size colors) $
  traceShow (showColors p d colors) $
  []
process (p, d, colors) (c: t: futureOutputs) =
  -- traceShow ("xy", toIntParts p,"out", out,robot d, "paint", c,"turn", t) $ 
  -- trace (showColors p d colors) $
  out: process (p', d', insert p c colors) futureOutputs
  where p' = p + d'
        d' = d * (0 :+ (1.0 - fromIntegral t * 2))
        out = findWithDefault 0 p' colors

showProcess :: [Integer] -> (Complex Float, Complex Float, Map (Complex Float) Integer) -> (Complex Float, Complex Float, Map (Complex Float) Integer)
showProcess [] s = s
showProcess [c] s = trace ("leftover " ++ show c) $ s
showProcess (c: t: r) (p, d, m) = showProcess r (p', d', insert p c m)
  where d' = d * (0 :+ (1.0 - fromIntegral t * 2 ))
        p' = p + d'

showColors :: Complex Float -> Complex Float -> Map (Complex Float) Integer -> [Char]
showColors p d cs
  -- | null cs = []
  -- | otherwise
  = traceShow ("showColors", p, robot d, "min", (x0,y0), "max", (x1,y1), "colors size", size cs) $ 
    unlines [line y | y <- reverse $ [y0 .. y1]]
  where line :: Int -> [Char]
        line y = [ (v (fromIntegral x :+ fromIntegral y) $ findWithDefault 3 (fromIntegral x :+ fromIntegral y) cs) | x <- [x0 .. x1]]
        pts = map fst $ toList cs
        xs = map (round . realPart) $ p: pts
        ys = map (round . imagPart) $ p: pts
        x0 :: Int
        x0 = minimum xs
        y0 :: Int
        y0 = minimum ys
        x1 :: Int
        x1 = maximum xs
        y1 :: Int
        y1 = maximum ys
        v :: Complex Float -> Integer -> Char
        v p' c | p == p' = robot d
               | p' == (0.0 :+ 0.0) && c== 0 = 'o'
               | p' == (0.0 :+ 0.0) && c== 1 = 'O'
               | c == 1 = '#'
               | c == 0 = ' ' -- '.' -- change to space for clarity
               | otherwise = ' '

robot :: Complex Float -> Char
robot c | x == 0 && y == 1 = '^'
        | x == 1 && y == 0 = '>'
        | x == 0 && y == -1 = 'V'
        | x == -1 && y == 0 = '<'
        where (x,y) = toParts c

startColors = singleton (0 :+ 0) 0
startColors2 = singleton (0 :+ 0) 1

main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  putStrLn "Part 1"
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings
  let output = run (0,0) instructions $ (startColors!(0 :+ 0)): process ((0 :+ 0),(0 :+ 1), startColors) output
  let (finalp, finald, colors) = showProcess output ((0 :+ 0), (0 :+ 1), startColors)
  putStrLn $ "done\n"
    -- ++ (unlines $ map show output)
    ++ (show $ length output)
    ++ show (finalp, finald) ++ "\n" ++ showColors finalp finald colors
  putStrLn "Part 2"
  let output = run (0,0) instructions $ (startColors2!(0 :+ 0)): process ((0 :+ 0),(0 :+ 1), startColors2) output
  let (finalp, finald, colors) = showProcess output ((0 :+ 0), (0 :+ 1), startColors2)
  putStrLn $ "done\n"
    -- ++ (unlines $ map show output)
    ++ (show $ length output)
    ++ show (finalp, finald) ++ "\n" ++ showColors finalp finald colors



