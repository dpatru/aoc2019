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

import UI.NCurses
import Data.Char (chr, ord)
-- import Data.Complex (Complex((:+)))
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
  output :: [Integer]}

computer0 = Computer { state = Ready, memory = empty, iptr = 0, base = 0, input = [], output = [] }

run :: Computer -> Computer -- (Integer, Integer) -> Instructions -> [Integer] -> [Integer]
run c
  | state c == Ready = run $ c {state = Running}
  | state c == Done = c
  | state c == Blocked = if input c == [] then c else run $ c {state = Running}
  | otherwise = -- running
    --traceShow (iptr c, base c, memory c) $ 
    case instr `mod` 100 of
      1 -> -- add
        run $ c {iptr = i+4, memory = insert (addr 3) (arg 1 + arg 2) instructions}
      2 -> -- multiply
        run $ c {iptr = i+4, memory = insert (addr 3) (arg 1 * arg 2) instructions}
      3 -> -- read input
        if (null $ input c)
        then c {state = Blocked}
        else run $ c {iptr = i+2, memory = insert (addr 1) (head $ input c) instructions, input = tail $ input c}
      4 -> -- output
        --traceShow (arg 1) $ 
        run $ c {iptr = i+2, output = output c ++ [arg 1]}
      5 -> -- jump-if-true
        run $ c {iptr = if arg 1 == 0 then i+3 else arg 2}
      6 -> -- jump-if-false
        run $ c {iptr = if arg 1 == 0 then arg 2 else i+3}
      7 -> -- less than
        run $ c {iptr = i+4, memory = insert (addr 3) (if arg 1 < arg 2 then 1 else 0) instructions}
      8 -> -- equals
        run $ c {iptr = i+4, memory = insert (addr 3) (if arg 1 == arg 2 then 1 else 0) instructions}
      9 -> -- set relative base
        run $ c {iptr = i+2, base = base c + arg 1}
      99 -> -- halt
        run $ c {state = Done}
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

-- instance (Show a) => Show (Complex a) where
--   show (C x y) = "C " ++ show x ++ " " ++ show y
  
-- instance (Eq a) => Eq (Complex a) where
--   (C a ai) == (C b bi) = a == b && ai == bi
  
-- instance (Eq a, Ord a) => Ord (Complex a) where
--   compare (C a ai) (C b bi) | a < b || a == b && ai < bi = LT
--                             | a > b || a == b && ai > bi = GT
--                             | otherwise = EQ

-- -- Note that we are using a plane where imaginary (y-axis) numbers
-- -- increase downward. I think this means that right and left are
-- -- reversed.

-- data Complex a = C a a

-- -- instance Num (Complex Int) where
-- --   abs c = error "Can't take abs of Complex Int"

-- instance Num a => Num (Complex a) where
--   (C x y) + (C u v) = C (x+u) (y+v)
--   (C x y) * (C u v) = C (x*u-y*v) (x*v+y*u)
--   fromInteger n = C (fromInteger n) 0
--   abs (C x y) = C (abs x) y -- sqrt $ x*x + y*y
--   signum (C x y) = C (signum x) 0
--   negate (C x y) = C (negate x) (negate y)

main = do
  -- [instructionFile] <- getArgs
  instructionStrings <- readFile "21.input.txt" -- instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings

  putStrLn "Part 1"
  let linesToAscii = map (fromIntegral . ord) . unlines
  -- Jumps are 4 spaces. Jump on an island if you can. # = ground, . = hole, _ = don't care
  -- Jump if any of the three spaces in front of you have holes and the fourth space is solid.
  -- @ A B C D
  -- _ . _ _ #
  -- _ _ . _ #
  -- _ _ _ . #
  -- ~(ABC)D -> ~((ABC)+D)
  let ls = [ "OR A J"
           , "AND B J"
           , "AND C J"
           , "NOT J J"
           , "AND D J"
           , "WALK"]
  
  -- let c = run $ computer0{memory = instructions, input=ls}
  let c = run $ computer0{memory = instructions}
  putStrLn $ map (chr . fromIntegral) $ output c
  putStrLn $ "Running with input: \n" ++ unlines ls
  let c2 = run $ c {input=linesToAscii ls, output=[]}
  let (msg, result) = span (< 128) $ output c2
  putStrLn $ map (chr . fromIntegral) msg ++ concatMap show result

  putStrLn "\n\nPart 2"
  -- Jump as before . . .
  -- @ABCD  EFGHI 
  -- _.__#  
  -- __._#  
  -- ___.#
  -- But check that you are not blocked after the jump.
  --        EFGHI 
  --        .__#_
  --        #___#
  --        ##___
  -- ~(ABC)D -> ~((ABC)+D)
  -- (~E)H + EI + EF = (~E)H + E(I+F) = E(F+I) + H
  
  let ls2 = [
              -- jump if a hole is ahead and the landing is open
              "OR A J"
            , "AND B J"
            , "AND C J"
            , "NOT J J"
            , "AND D J"
              -- and if the future looks ok
            , "OR F T"
            , "OR I T"
            , "AND E T"
            , "OR H T"
            , "AND T J"
            , "RUN"
            ]
  let c3 = run $ computer0{memory = instructions}
  putStrLn $ map (chr . fromIntegral) $ output c3
  putStrLn $ "Running with input: \n" ++ unlines ls2
  let c4 = run $ c3 {input=linesToAscii ls2, output=[]}
  let (msg, result) = span (< 128) $ output c4
  putStrLn $ map (chr . fromIntegral) msg ++ concatMap show result
