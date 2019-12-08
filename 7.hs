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
  let instr = instructions!i in
    case instr `mod` 100 of
      1 -> --("add: " ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                -- ("  arg1="++show arg1):
                -- ("  arg2="++show arg2):
                -- ("  arg3="++show arg3):
                run (i+4) (instructions//[(arg3, arg1 + arg2)]) inputs)
      2 -> --("mult" ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, arg1 * arg2)]) inputs)
      3 -> --("input: " ++ show [instructions!j| j<-[i .. i+1]]):
        (let input1 = head inputs in
           let arg1 = instructions!(i+1) in 
             --("  storing "++input1++" at "++ show arg1): 
                (run (i+2) (instructions//[(arg1, input1)]) (tail inputs)))
      4 -> let arg1 = if instr `mod` 1000 < 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
        --("output: " ++ show [instructions!j| j<-[i .. i]] ++ ": " ++ show arg1) :
        arg1 : 
        (run (i+2) instructions inputs )
      5 -> --("jump-if-true: " ++ show [instructions!j| j<-[i .. i+1]]):
        (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
            let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
              run (if arg1 == 0 then i+3 else arg2) instructions inputs)
      6 -> --("jump-if-false" ++ show [instructions!j| j<-[i .. i+2]]):
        (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
            let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
              run (if arg1 == 0 then arg2 else i+3) instructions inputs)
      7 -> --("less than" ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, if arg1 < arg2 then 1 else 0)]) inputs)
      8 -> --("equals" ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, if arg1 == arg2 then 1 else 0)]) inputs)
      99 -> --": stop":
        []
      _ -> error "unknown opcode"
  

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
  let thruster phase = amps instructions phase 0
  let (p, s) = argmaxWithMax thruster $ permutations [0 .. 4]
  putStrLn $ "Phase " ++ (concat $ map show p) ++ ", thruster signal " ++ show s
                          


