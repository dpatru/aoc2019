#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Matrix.Vector (fromList)
import Data.Array (Array, (!), (//), elems)
--import qualified Data.Array as A
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.List.Extras.Argmax (argmaxWithMax)

run :: Int -> Array Int Int -> [Int] -> [Int]-- instructionPointer, instructions, input, output
-- run i instructions inputs = unlines inputs
run i instructions inputs =
  case instr `mod` 100 of
    1 -> run (i+4) (instructions//[(arg3, arg1 + arg2)]) inputs
    2 -> run (i+4) (instructions//[(arg3, arg1 * arg2)]) inputs
    3 -> run (i+2) (instructions//[(arg1, head inputs)]) (tail inputs)
    4 -> arg1: run (i+2) instructions inputs
    5 -> run (if arg1 == 0 then i+3 else arg2) instructions inputs
    6 -> run (if arg1 == 0 then arg2 else i+3) instructions inputs
    7 -> run (i+4) (instructions//[(arg3, if arg1 < arg2 then 1 else 0)]) inputs
    8 -> run (i+4) (instructions//[(arg3, if arg1 == arg2 then 1 else 0)]) inputs
    99 -> []
    _ -> error "unknown opcode"
  where instr = instructions!i
        arg1 = arg 1
        arg2 = arg 2
        arg3 = arg 3
        arg :: Int -> Int
        arg n = if instr `mod` 100*10^n <= 10*10^n then instructions!(instructions!(i+n)) else instructions!(i+n)

amps :: Array Int Int -> [Int] -> Int -> Int 
amps instructions [] input = input
amps instructions (p:phases) input
  | length outputs == 1
  = amps instructions phases (head outputs)
  | otherwise
  = error "muliple outputs"
  where outputs = run 0 instructions [p, input]

main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . map read $ splitOn "," instructionStrings
  let feedback :: [Int] -> Int -- reversed phase to last output
      feedback phase = last output
        where output = foldr r (0: output) phase
              r p o = run 0 instructions $ (p:o) -- run computer on input (p:o) 
              -- output = r $ p_n : r $ p_n-1 $ . . . r $ p0 : 0 : output
              -- -> run machine on input p_i and the output of the machine i-1. Machine 0 gets as input p_0, 0, and the output of the first machine n. 
           
  let (p, s) = argmaxWithMax feedback $ permutations [5 .. 9]
  --let thruster phase = amps instructions phase 0
  --let (p, s) = argmaxWithMax thruster $ permutations [0 .. 4]
  putStrLn $ "Phase " ++ (reverse $ concat $ map show p)
    ++ ", thruster signal " ++ show s
                          


