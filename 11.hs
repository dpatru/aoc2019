#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Map.Strict (Map, (!), insert, elems, fromList, toList, findWithDefault, size, empty, member)
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



process :: ((Integer, Integer), (Integer, Integer), Map (Integer, Integer) Integer) -> [Integer] -> [Integer]
process (p, d, colors) [] = traceShow (size colors) []
process (p, d, colors) (c: t: futureOutputs) = traceShow ("\nposition: " ++ show p ++ "; colors: " ++ (show $ size colors)) $ 
  out: process (p', d', insert p c colors) futureOutputs
  where p' = (fst p + fst d', snd p + snd d')
        d' | d == (0,1) && t == 0 = (-1, 0) -- up, turn left -> left
           | d == (1,0) && t == 0 = (0, 1) -- right, turn left -> up
           | d == (0,-1) && t == 0 = (1, 0) -- down, turn left -> right
           | d == (-1,0) && t == 0 = (0, -1) -- left, turn left -> down
           | d == (0,1) && t == 1 = (1, 0) -- up, turn right -> right
           | d == (1,0) && t == 1 = (0, -1) -- right, turn right -> down
           | d == (0,-1) && t == 1 = (-1, 0) -- down, turn right-> left
           | d == (-1,0) && t == 1 = (0, 1) -- left, turn right -> up
        out = if d' `member` colors then colors!d' else 0
        
main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings
  let output = run (0,0) instructions (0: process ((0,0),(0,1),empty) output)
  putStrLn $ "done" ++ show output



