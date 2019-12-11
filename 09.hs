#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Map.Strict (Map, (!), insert, elems, fromList, toList, findWithDefault)
--import qualified Data.Array as A
import Data.List (permutations)
import Data.List.Split (splitOn)

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
  
main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings
  interact $ unlines . map show . run (0, 0) instructions . map read . lines
  -- putStrLn "Part 1"
  -- let no_feedback phase = foldr r [0] phase
  --       where r p o = run 0 instructions (p:o)
  -- let (s, p) = maximum [(no_feedback p, p) | p <- permutations [0 .. 4]]
  -- putStrLn $ "Phase " ++ (reverse $ concat $ map show p)
  --   ++ ", thruster signal " ++ show s

  -- putStrLn "\nPart 2"
  -- let feedback :: [Integer] -> Integer -- reversed phase to last output
  --     feedback phase = last output
  --       where output = foldr r (0: output) phase
  --             r p o = run 0 instructions $ (p:o)
  --             -- run computer on input (p:o) output =
  --             -- r $ p_n : r $ p_n-1 $ . . . r $ p0 : 0 : output

  --             -- This runs machine_i on input p_i and the output of the
  --             -- machine i-1. Machine 0 gets as input p_0, 0, and the
  --             -- output of the "first" machine n.
           
  -- let (s, p) = maximum [(feedback p, p) | p <- permutations [5 .. 9]]
  -- putStrLn $ "Phase " ++ (reverse $ concat $ map show p)
  --   ++ ", thruster signal " ++ show s



