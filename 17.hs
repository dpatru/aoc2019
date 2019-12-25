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

instance (Show a) => Show (Complex a) where
  show (C x y) = "C " ++ show x ++ " " ++ show y
  
instance (Eq a) => Eq (Complex a) where
  (C a ai) == (C b bi) = a == b && ai == bi
  
instance (Eq a, Ord a) => Ord (Complex a) where
  compare (C a ai) (C b bi) | a < b || a == b && ai < bi = LT
                            | a > b || a == b && ai > bi = GT
                            | otherwise = EQ

-- Note that we are using a plane where imaginary (y-axis) numbers
-- increase downward. I think this means that right and left are
-- reversed.

data Complex a = C a a

-- instance Num (Complex Int) where
--   abs c = error "Can't take abs of Complex Int"

instance Num a => Num (Complex a) where
  (C x y) + (C u v) = C (x+u) (y+v)
  (C x y) * (C u v) = C (x*u-y*v) (x*v+y*u)
  fromInteger n = C (fromInteger n) 0
  abs (C x y) = C (abs x) y -- sqrt $ x*x + y*y
  signum (C x y) = C (signum x) 0
  negate (C x y) = C (negate x) (negate y)

  
type Point = Complex Int
type Direction = Complex Int
type Scaffold = Set Point
data Move = R | L | F Int | Err
  deriving (Show, Eq)

show' :: [Move] -> String
show' ms = intercalate "," $ map showMove ms
  where showMove R = "R"
        showMove L = "L"
        showMove (F n) = show n
        showMove Err = error "showing err"
        
charToDirection '>' = C 1 0
charToDirection '<' = C (-1) 0
charToDirection '^' = C 0 (-1)
charToDirection 'v' = C 0 1
charToDirection c = error $ "bad direction" ++ [c]

fromJust' :: String -> Maybe a -> a
fromJust' s Nothing = error s
fromJust' s (Just x) = x

path :: Scaffold -> (Point, Direction) -> [Move]
path scaffold (pt, dir)
  | S.size scaffold < 2 = []
  -- | otherwise = (traceShowId $ rotation ++ [F steps]) ++ path scaffold' (pt', dir')
  | otherwise = rotation ++ [F steps] ++ path scaffold' (pt', dir')
  where left :: Complex Int
        left = C 0 (-1)
        dir' = fromJust' "dir'" $ find (\d -> (pt + d) `S.member` scaffold) [left^i | i <- [1 .. 4]]
        directions = [(left, [L]), (left^2, [L,L]), (left^3, [R]), (left^4, [Err])]
        rotation = snd $ fromJust' ("rotation: "++ show (dir', directions))  $
          find (\(d, m) -> dir * d == dir') directions
        (pt', steps, scaffold') = walk (pt, 0, scaffold)
        walk (p, n, s) | not $ (p+dir') `S.member` s = (p, n, s)
                       | (p+dir'*left) `S.member` s = walk (p+dir', n+1, s) -- intersection, don't delete
                       | otherwise = walk (p+dir', n+1, S.delete p s)

testIntersection scaffold pt = all (`S.member` scaffold) [ pt + d | d <- [l, l*l, l*l*l, l*l*l*l]]
  where l = C 0 1


data Function = A | B | C' deriving (Show)

substitute :: [Move] -> [Function]
-- substitute (L: F n: ms) = A: (substitute $ F (n-8): ms)
-- substitute (F n: ms) = B n: substitute ms
-- substitute (R: F n: ms) = C': (substitute $ F (n-8): ms)
-- substitute [] = []
-- substitute ms = error $ "substitute: can't do " ++ show ms
substitute (L: F 12: L: F 12: R: F 12: ms) = A: substitute ms
substitute (L: F 8: L: F 8: R: F 12: L: F 8: L: F 8: ms) = B: substitute ms
substitute (L: F 10: R: F 8: R: F 12: ms) = C': substitute ms
substitute [] = []
substitute ms = error $ "substitute: can't do " ++ show ms

main = do
  -- [instructionFile] <- getArgs
  instructionStrings <- readFile "17.input.txt" -- instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings

  putStrLn "Part 1"
  let c = run $ computer0{memory = instructions}
  let view = output c
  let viewStr = map (chr . fromIntegral) view
  putStrLn $ viewStr
  let listView = [(C i j, c) | (j, line) <- zip [0 ..] (lines viewStr)
                              , (i, c) <- zip [0 ..] line
                              , c `elem` "#<>^vV"]

  let scaffold = S.fromList $ map fst listView

  let intersections = S.filter (testIntersection scaffold) scaffold
  print $ S.foldl' (\s (C x y) -> s+x*y) 0 intersections

  putStrLn "Part 2"
  let (r, d) = fromJust' "find robot" $ find (\(pt, v) -> v `elem` "<>^vV") listView
  let p = path scaffold (r, charToDirection d)
  -- print p
  -- let cs = substitute p
  -- print cs
  -- print $ length cs
  let ls = map (fromIntegral . ord) $ unlines
        [ "A,A,B,C,C,A,B,C,A,B"
        , "L,12,L,12,R,12"
        , "L,8,L,8,R,12,L,8,L,8"
        , "L,10,R,8,R,12"
        , "n"
        ]
  print ls
  -- let c = run $ computer0 { memory = insert 0 2 instructions
  --                         , input = ls }

  -- print $ output c

  print "Part 2 - alt"
  print p
  print $ length p
  let substrings xs = concatMap inits $ tails xs
  let functions = Prelude.filter (\l -> length l > 1 && (length $ show' l) < 21) $ substrings p
  print $ length $ functions
  -- let functions3 = [(a,b,c) | a <- functions, b <- functions, show' a < show' b, c <- functions, show' b < show' c]
  let f xs ys = show' xs < show' ys
  let functions3 = [[a,b,c] | a <- inits $ take 10 p, length a > 0, c <- functions, b <- Prelude.filter (f c) functions]
  print $ length $ functions3

  -- print $ Prelude.filter (\[a,b,c] -> (length $ show' a) == 14 && (length $ show' b) == 13 && (length $ show' c) == 20) functions3
  let test3 [] fs = True
      test3 pth fs = any id [test3 (drop (length f) pth) fs | f <- fs, isPrefixOf f pth]
  let goodFunctions = Prelude.filter (test3 p) $ functions3
  let goodFunction = head $ goodFunctions
  print $ goodFunction
  let programs [] fs = [[]]
      programs pth fs = [ (l: ls) | (l, f) <- zip ['A' .. 'C'] fs
                                  , f `isPrefixOf` pth
                                  , ls <- programs (drop (length f) pth) fs
                                  ]
  let program = intersperse ',' $ head $ programs p goodFunction
  let functions = map show' goodFunction
  print program
  print functions
  let ls = unlines ((program: functions) ++ ["n"])
  print ls
  print $ map (fromIntegral . ord) ls
  let c = run $ computer0 { memory = insert 0 2 instructions
                          , input = map (fromIntegral . ord) ls }

  print $ last $ output c
  print $ map (chr . fromIntegral) $ init $ output c
  

-- After first run:
-- [A,B 4,
--  A,B 4,
--  C',B 4,

--  A,B 4,
--  A,B 4,
--  C',B 4,

--  A,B 0,
--  A,B 0,
--  C',B 4,
--  A,B 0,
--  A,B 0,

--  A,B 2,
--  C',B 0,
--  C',B 4,

--  A,B 2,
--  C',B 0,
--  C',B 4,

--  A,B 4,
--  A,B 4,
--  C',B 4,

--  A,B 0,
--  A,B 0,
--  C',B 4,
--  A,B 0,
--  A,B 0,

--  A,B 2,
--  C',B 0,
--  C',B 4,

--  A,B 4,
--  A,B 4,
--  C',B 4,

--  A,B 0,
--  A,B 0,
--  C',B 4,
--  A,B 0,
--  A,B 0]
