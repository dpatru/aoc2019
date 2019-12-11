#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Matrix.Vector (fromList)
import Data.Array (Array, (!), (//), elems)
--import qualified Data.Array as A
import Data.List.Split (splitOn)

-- run :: Int -> Array Int Int -> [String] -> [String]-- instructionPointer, instructions, input, output
-- -- run i instructions inputs = unlines inputs
-- run i instructions inputs =
--   let instr = instructions!i in
--     case instr `mod` 100 of
--       1 -> ("add: " ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
--         (let arg3 = instructions!(i+3) in 
--             let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
--               let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
--                 ("  arg1="++show arg1):
--                 ("  arg2="++show arg2):
--                 ("  arg3="++show arg3):
--                 run (i+4) (instructions//[(arg3, arg1 + arg2)]) inputs)
--       2 -> ("mult" ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
--         (let arg3 = instructions!(i+3) in 
--             let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
--               let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
--                 run (i+4) (instructions//[(arg3, arg1 * arg2)]) inputs)
--       3 -> ("input: " ++ show [instructions!j| j<-[i .. i+1]]):
--         (let input1 = head inputs in
--            let arg1 = instructions!(i+1) in 
--              ("  storing "++input1++" at "++ show arg1): 
--                 (run (i+2) (instructions//[(arg1, read input1)]) (tail inputs)))
--       4 -> let arg1 = if instr `mod` 1000 < 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
--         ("output: " ++ show [instructions!j| j<-[i .. i]] ++ ": " ++ show arg1) :
--         (run (i+2) instructions inputs )
--       5 -> ("jump-if-true: " ++ show [instructions!j| j<-[i .. i+1]]):
--         (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
--             let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
--               run (if arg1 == 0 then i+3 else arg2) instructions inputs)
--       6 -> ("jump-if-false" ++ show [instructions!j| j<-[i .. i+2]]):
--         (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
--             let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
--               run (if arg1 == 0 then arg2 else i+3) instructions inputs)
--       7 -> ("less than" ++ show [instructions!j| j<-[i .. i+3]]):
--         (let arg3 = instructions!(i+3) in 
--             let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
--               let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
--                 run (i+4) (instructions//[(arg3, if arg1 < arg2 then 1 else 0)]) inputs)
--       8 -> ("equals" ++ show [instructions!j| j<-[i .. i+3]]):
--         (let arg3 = instructions!(i+3) in 
--             let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
--               let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
--                 run (i+4) (instructions//[(arg3, if arg1 == arg2 then 1 else 0)]) inputs)
--       99 -> ": stop": []
--       _ -> error "unknown opcode"
  
-- putLn s = s ++ "\n"

-- main2 = do
--   [instructionFile] <- getArgs
--   instructions <- readFile instructionFile
--   interact $ unlines . run 0 (fromList . map read $ splitOn "," instructions) . lines
                          
-- main3 = interact $ evalState get -- run the state monad 'get' which simply returns the statee

-- main4 = interact $ unlines . evalState get . lines

-- main5 = interact $ unlines . evalState (do s <- get; return $ map (\l-> "line: "++l) s) . lines
-- main = main5

run :: Int -> Array Int Int -> State [String] [String]-- instructionPointer, instructions, State input output
-- run i instructions inputs = unlines inputs
run i instructions = do
  case instr `mod` 100 of
    1 -> ("add: " ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
      (let arg3 = instructions!(i+3) in 
         let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
           let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
             ("  arg1="++show arg1):
             ("  arg2="++show arg2):
             ("  arg3="++show arg3):
             run (i+4) (instructions//[(arg3, arg1 + arg2)]) inputs)
      2 -> ("mult" ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, arg1 * arg2)]) inputs)
      3 -> ("input: " ++ show [instructions!j| j<-[i .. i+1]]):
        (let input1 = head inputs in
           let arg1 = instructions!(i+1) in 
             ("  storing "++input1++" at "++ show arg1): 
                (run (i+2) (instructions//[(arg1, read input1)]) (tail inputs)))
      4 -> let arg1 = if instr `mod` 1000 < 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
        ("output: " ++ show [instructions!j| j<-[i .. i]] ++ ": " ++ show arg1) :
        (run (i+2) instructions inputs )
      5 -> ("jump-if-true: " ++ show [instructions!j| j<-[i .. i+1]]):
        (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
            let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
              run (if arg1 == 0 then i+3 else arg2) instructions inputs)
      6 -> ("jump-if-false" ++ show [instructions!j| j<-[i .. i+2]]):
        (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
            let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
              run (if arg1 == 0 then arg2 else i+3) instructions inputs)
      7 -> ("less than" ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, if arg1 < arg2 then 1 else 0)]) inputs)
      8 -> ("equals" ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, if arg1 == arg2 then 1 else 0)]) inputs)
      99 -> ": stop": []
      _ -> error "unknown opcode"
  
  let instr = instructions!i in
    case instr `mod` 100 of
      1 -> ("add: " ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                ("  arg1="++show arg1):
                ("  arg2="++show arg2):
                ("  arg3="++show arg3):
                run (i+4) (instructions//[(arg3, arg1 + arg2)]) inputs)
      2 -> ("mult" ++ show i ++ ": " ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, arg1 * arg2)]) inputs)
      3 -> ("input: " ++ show [instructions!j| j<-[i .. i+1]]):
        (let input1 = head inputs in
           let arg1 = instructions!(i+1) in 
             ("  storing "++input1++" at "++ show arg1): 
                (run (i+2) (instructions//[(arg1, read input1)]) (tail inputs)))
      4 -> let arg1 = if instr `mod` 1000 < 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
        ("output: " ++ show [instructions!j| j<-[i .. i]] ++ ": " ++ show arg1) :
        (run (i+2) instructions inputs )
      5 -> ("jump-if-true: " ++ show [instructions!j| j<-[i .. i+1]]):
        (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
            let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
              run (if arg1 == 0 then i+3 else arg2) instructions inputs)
      6 -> ("jump-if-false" ++ show [instructions!j| j<-[i .. i+2]]):
        (let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
            let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
              run (if arg1 == 0 then arg2 else i+3) instructions inputs)
      7 -> ("less than" ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, if arg1 < arg2 then 1 else 0)]) inputs)
      8 -> ("equals" ++ show [instructions!j| j<-[i .. i+3]]):
        (let arg3 = instructions!(i+3) in 
            let arg2 = if instr `mod` 10000 <= 1000 then instructions!(instructions!(i+2)) else instructions!(i+2) in 
              let arg1 = if instr `mod` 1000 <= 100 then instructions!(instructions!(i+1)) else instructions!(i+1) in
                run (i+4) (instructions//[(arg3, if arg1 == arg2 then 1 else 0)]) inputs)
      99 -> ": stop": []
      _ -> error "unknown opcode"
  
main = interact $ unlines . evalState (do s <- get; return $ map (\l-> "line: "++l) s) . lines
