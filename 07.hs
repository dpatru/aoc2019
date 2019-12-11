#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

-- import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Matrix.Vector (fromList)
import Data.Array (Array, (!), (//), elems)
--import qualified Data.Array as A
import Data.List (permutations)
import Data.List.Split (splitOn)

run :: Int -> Array Int Int -> [Int] -> [Int]-- instructionPointer, instructions, input, output
-- run i instructions inputs = unlines inputs
run i instructions inputs =
  --trace ((show $ elems instructions) ++ ": " ++ show i ++ ": " ++ show instr ++ ": input "++show inputs ) $
  case instr `mod` 100 of
    1 -> run (i+4) (instructions//[(addr 3, arg 1 + arg 2)]) inputs
    2 -> run (i+4) (instructions//[(addr 3, arg 1 * arg 2)]) inputs
    3 -> run (i+2) (instructions//[(addr 1, head inputs)]) (tail inputs)
    4 -> arg 1: run (i+2) instructions inputs
    5 -> run (if arg 1 == 0 then i+3 else arg 2) instructions inputs
    6 -> run (if arg 1 == 0 then arg 2 else i+3) instructions inputs
    7 -> run (i+4) (instructions//[(addr 3, if arg 1 < arg 2 then 1 else 0)]) inputs
    8 -> run (i+4) (instructions//[(addr 3, if arg 1 == arg 2 then 1 else 0)]) inputs
    99 -> []
    _ -> error "unknown opcode"
  where instr = instructions!i
        arg :: Int -> Int
        arg n = if instr `mod` (100*10^n) <= 10*10^n
                then instructions!(instructions!(i+n))
                else instructions!(i+n)
        addr n = instructions!(i+n)
  
main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . map read $ splitOn "," instructionStrings

  putStrLn "Part 1"
  let no_feedback phase = foldr r [0] phase
        where r p o = run 0 instructions (p:o)
  let (s, p) = maximum [(no_feedback p, p) | p <- permutations [0 .. 4]]
  putStrLn $ "Phase " ++ (reverse $ concat $ map show p)
    ++ ", thruster signal " ++ show s

  putStrLn "\nPart 2"
  let feedback :: [Int] -> Int -- reversed phase to last output
      feedback phase = last output
        where output = foldr r (0: output) phase
              r p o = run 0 instructions $ (p:o)
              -- run computer on input (p:o) output =
              -- r $ p_n : r $ p_n-1 $ . . . r $ p0 : 0 : output

              -- This runs machine_i on input p_i and the output of the
              -- machine i-1. Machine 0 gets as input p_0, 0, and the
              -- output of the "first" machine n.
           
  let (s, p) = maximum [(feedback p, p) | p <- permutations [5 .. 9]]
  putStrLn $ "Phase " ++ (reverse $ concat $ map show p)
    ++ ", thruster signal " ++ show s



