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
import Data.List (find, intercalate, intersperse, permutations, inits, tails, isPrefixOf, sort, subsequences)
import Data.List.Split (splitOn)
-- import Data.Complex (Complex((:+)), realPart, imagPart) -- define my own complex

import UI.NCurses
import Data.Char (chr, ord, isLower, isUpper, toUpper)
-- import Data.Complex (Complex((:+)))
import Data.Maybe (fromJust)



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
  abs (C x y) = C (abs $ x + y) 0 -- manhattan distance -- sqrt $ x*x + y*y
  signum (C x y) = C (signum x) 0
  negate (C x y) = C (negate x) (negate y)

  
type Point = Complex Int
type Direction = Complex Int
type Scaffold = Set Point

fromJust' :: String -> Maybe a -> a
fromJust' s Nothing = error s
fromJust' s (Just x) = x

type Maze = Map (Complex Int) Char
type Search = (Int, Int, Int, Complex Int, Maze, Set (Complex Int), [Char], Bool)
-- rank, steps, remaining keys, location, maze, seen, collectedKeys, newKeyFound
-- Search is a tuple because we will be putting it into a heap (Set) and extracting one with the minimum steps
type Heap = Set Search

neighbors :: Complex Int -> [Complex Int]
neighbors x = [ x + (C 0 1)
              , x + (C 0 (-1))
              , x + (C 1 0)
              , x + (C (-1) 0)
              ]
              
frontier :: Search -> [Search]
frontier (rank, steps, remainingKeys, position, maze, seen, collectedKeys, newKeyFound)
  | newKeyFound || (not $ isLower v)
  = [(steps'+remainingKeys', steps', remainingKeys', n, maze', seen', collectedKeys, False)
    | n <- neighbors position
    , n `member` maze -- exclude points not in the maze, before trying to get their value
    , not $ isUpper $ maze ! n
    , not $ n `S.member` seen'
    ]
  | isLower v = [(rank, steps, remainingKeys, position, maze, seen, (sort $ v:collectedKeys), True)]
  | otherwise = error "bad frontier case"
  where steps' = steps + 1
        v = maze ! position -- already confirmed in the maze
        remainingKeys' = if isLower v then remainingKeys - 1 else remainingKeys
        maze' = if isLower v
                then M.map (\c -> if c == v || c == toUpper v then '.' else c) maze
                else maze
        seen' = if isLower v then S.empty else S.insert position seen

search :: Set Search -> Set String -> Search
search heap benchmarks
  | S.null heap = error "empty heap"
  | remainingKeys == 0 = best
  | newKeyFound && collectedKeys `S.member` benchmarks = search heap'' benchmarks
  | newKeyFound = traceShow (collectedKeys, steps) $
    search heap' $ foldl (\b ks -> S.insert ks b) benchmarks $ subsequences collectedKeys
  | otherwise = -- traceShow (S.size heap, steps, remainingKeys) $
    search heap'' benchmarks
  where (best@(rank, steps, remainingKeys, position, maze, seen, collectedKeys, newKeyFound), heap')
          = S.deleteFindMin heap
        heap'' = foldl (\h s -> S.insert s h) heap' $ frontier best
        
main = do
  -- [instructionFile] <- getArgs
  mazeString <- readFile "18.input.txt" -- instructionFile
  let mazeStrings = lines mazeString
  let maze = fromList [(C i j, c) | (line, j) <- zip mazeStrings (reverse [0 .. (length mazeStrings - 1)])
                                  , (c, i) <- zip line [0 .. ]
                                  , c /= '#'
                                  ]
  let numberOfKeys = foldl (\s c -> if isUpper c then s+1 else s) 0 mazeString
  let startPosition = head [p | (p,v) <- toList maze, v == '@']
  print ("number of keys", numberOfKeys, "start", startPosition)
  
  print $ take 10 $ toList maze

  putStrLn "Part 1"
    -- Search the maze by expanding the frontier one step at a time in
    -- each direction. Throw all the frontiers on Don't allow
    -- backtracking until you consume a key, then reset the
    -- backtracking set.
  let searchEnd@(rank, steps, remainingKeys, position, maze', seen', keysCollected, newKeyFound)
        = search (S.singleton (numberOfKeys, 0, numberOfKeys, startPosition, maze, S.empty, "", False)) S.empty
  print $ steps
  
  putStrLn "Part 2"
