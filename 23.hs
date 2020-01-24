{-# LANGUAGE FlexibleInstances #-}

import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Map.Strict (Map, (!), insert, elems, fromList, toList, findWithDefault, size, empty, member, findMin, findMax, singleton, filter)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
--import qualified Data.Array as A
import Data.List (find, intercalate, intersperse, permutations, inits, tails, isPrefixOf)
import Data.List.Split (splitOn)
-- import Data.Complex (Complex((:+)), realPart, imagPart) -- define my own complex

-- import UI.NCurses
-- import Data.Char (chr, ord)
-- -- import Data.Complex (Complex((:+)))
import Data.Maybe (fromJust)
type Instructions = Map Integer Integer

data ComputerState = Ready | Running | Blocked | Done
  deriving (Show, Eq)

data Computer = Computer {
  state :: ComputerState,
  memory :: Map Integer Integer,
  iptr :: Integer, -- instruction pointer
  base :: Integer, -- base offset
  input :: [Integer],
  output :: [Integer],
  time :: Integer}

computer0 = Computer { state = Ready, memory = empty, iptr = 0, base = 0, input = [], output = [], time = 0 }

run1 :: Computer -> Computer -- (Integer, Integer) -> Instructions -> [Integer] -> [Integer]
run1 c
  | state c == Ready = run1 $ c {state = Running}
  | state c == Done = c
  | state c == Blocked = if null (input c) then c else run1 $ c {state = Running}
  | otherwise = -- running
    --traceShow (iptr c, base c, memory c) $ 
    case instr `mod` 100 of
      1 -> -- add
        addTime $ c {iptr = i+4, memory = insert (addr 3) (arg 1 + arg 2) instructions}
      2 -> -- multiply
        addTime $ c {iptr = i+4, memory = insert (addr 3) (arg 1 * arg 2) instructions}
      3 -> -- read input
        if (null $ input c)
        then c {state = Blocked}
        else addTime $ c {iptr = i+2, memory = insert (addr 1) (head $ input c) instructions, input = tail $ input c}
      4 -> -- output
        --traceShow (arg 1) $ 
        addTime $ c {iptr = i+2, output = output c ++ [arg 1]}
      5 -> -- jump-if-true
        addTime $ c {iptr = if arg 1 == 0 then i+3 else arg 2}
      6 -> -- jump-if-false
        addTime $ c {iptr = if arg 1 == 0 then arg 2 else i+3}
      7 -> -- less than
        addTime $ c {iptr = i+4, memory = insert (addr 3) (if arg 1 < arg 2 then 1 else 0) instructions}
      8 -> -- equals
        addTime $ c {iptr = i+4, memory = insert (addr 3) (if arg 1 == arg 2 then 1 else 0) instructions}
      9 -> -- set relative base
        addTime $ c {iptr = i+2, base = base c + arg 1}
      99 -> -- halt
        addTime $ c {state = Done}
      _ -> error "unknown opcode"
  where instructions = memory c
        i = iptr c
        instr = instructions!i
        ii x = findWithDefault 0 x instructions
        arg :: Integer -> Integer
        arg n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ ii $ i+n
          1 -> ii $ i+n
          2 -> ii ((ii $ i+n) + base c)
          _ -> error "bad argument mode"
        addr n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ i+n
          1 -> error "address in mode 1"
          2 -> --trace "address in mode 2" $
            ii (i+n) + base c
          _ -> error $ "address in unknown mode"
        addTime c = c{time = time c + 1}

run :: Computer -> Computer
run c | state c == Done = c
      | state c == Blocked = if null (input c) then c else run $ run1 c
      | length (output c) == 3 = c
      | otherwise = run $ run1 c
      
runNetwork :: Map Int Computer -> Set (Integer, Int) -> Integer
runNetwork computers heap
  | isDone = y
  | otherwise = runNetwork computers'' heap''
  where
    ((_, i), heap') = S.deleteFindMin heap -- find the computer with lowest time
    c = computers!i
    hasOutput = 3 <= length (output c)
    [addr_, x, y] = take 3 $ output c -- laziness means this won't be executed unless needed
    addr = fromIntegral addr_
    isDone = hasOutput && addr == 255
    c' = if hasOutput then c{output = drop 3 $ output c} else c
    c'' = if state c' == Blocked && null (input c') then c'{input = [-1]} else c'
    c''' = run c''
    c2 = computers!addr
    computers' = if hasOutput then M.insert addr (c2{input = input c2 ++ [x,y]}) computers else computers
    computers'' = M.insert i c''' computers'
    heap'' = S.insert (time c''', i) heap'

main = do
  -- [instructionFile] <- getArgs
  instructionStrings <- readFile "23.input.txt" -- instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings

  putStrLn "Part 1"
  let addrs = [0 .. 49]
      computers = M.fromList [(i, computer0{memory=instructions, input=[fromIntegral i]}) | i <- addrs]
      heap = S.fromList [(0,i) | i <- addrs]
  print $ runNetwork computers heap
