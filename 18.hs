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
import Data.Char (chr, ord, isLower, isUpper, toUpper, isDigit)
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

neighbors :: Complex Int -> [Complex Int]
neighbors x = [ x + (C 0 1)
              , x + (C 0 (-1))
              , x + (C 1 0)
              , x + (C (-1) 0)
              ]

type Maze = Map Point Char
strToMaze :: String -> Maze
strToMaze s = fromList [(C i j, c) | (l, j) <- zip ls (reverse [0 .. (rows - 1)])
                                   , (c, i) <- zip l [0 ..]
                                   , c /= '#']
  where ls = lines s
        rows = length ls

-- optimize maze for lots of hallways
type Cost = Int
type MazeGraph' = Map Point (Map Point Cost) -- Graph, from pt to neighbors, with cost, eliminates hallways
mazeToGraph' :: Maze -> MazeGraph'
mazeToGraph' m = foldl graph M.empty $ [ pt
                                       | (pt, c) <- M.toList m
                                       , isLower c || isUpper c || isDigit c || c == '@']
  where graph g pt = addNeighbors (g, S.empty) $ S.singleton (0, pt)
          where addNeighbors :: (MazeGraph', Set Point) -> Set (Cost, Point) -> MazeGraph'
                addNeighbors (g, seen) heap -- note that seen does not
                                            -- include the
                                            -- destinations (locations
                                            -- marked with letters or
                                            -- the start)
                  | null heap -- no more neighbors to process, we're done
                  = g
                  | neighbor == pt -- initial case, add true neighbors to the heap
                  = --traceShow ("starting with ", pt) $
                  addNeighbors (g, seen') heap''
                  | neighbor `S.member` seen -- we've already seen this neighbor, skip
                  = addNeighbors (g, seen) heap'
                  | m!neighbor == '.'  -- we're in a passage, add neighbor's neighbors to the heap
                  = --traceShow ("in hall", pt, neighbor) $
                  addNeighbors (g, seen') heap''
                  | otherwise -- we've reached a destination, add it to the graph
                  = traceShow ("adding", m!pt, m!neighbor) $
                  addNeighbors (g', seen) heap'
                  where ((pathLength, neighbor), heap') = S.deleteFindMin heap
                        seen' = neighbor `S.insert` seen
                        heap'' = heap' `S.union`
                          S.fromList [(pathLength+1, n) | n <- neighbors neighbor
                                                        , not $ n `S.member` seen
                                                        , n `M.member` m]
                        g' = M.unionWith (M.union) g $
                             fromList[ (pt, M.fromList [(neighbor, pathLength)])
                                     , (neighbor, M.fromList [(pt, pathLength)]) ]

mazeToPositions :: Maze -> Map Char Point
mazeToPositions m = fromList [(c, pt) | (pt, c) <- toList m, isLower c || isUpper c || c == '@']

type MazeGraph = Map Char (Map Char Cost) -- Graph, from char to neighbors, with cost, eliminates hallways
mazeToGraph m = foldl addPt M.empty $ toList g
  where g = mazeToGraph' m
        addPt g (pt, nbrs) = M.insertWith M.union (m!pt) nbrs' g
          where nbrs' = fromList [(m!k, v) | (k,v) <- toList nbrs]


-- We want to collect all the keys in the shortest path, where the
-- maze can change after each key is collected. We can solve this
-- using a search routine that takes a starting point and a
-- particular maze, and finds all the next keys reachable from that
-- point. For example, suppose that starting at @, we can get to
-- keys a and b only. Then, we can search from these points to the
-- next reachable points. 

-- How much does this search cost? Let's overestimate, and assume
-- that all 26 keys are reachable at every stage. Then we have 26
-- keys from which to pick first, then for each pick, we have 25
-- keys from which to pick the second key, then for each second key,
-- we have 24 ways to pick the third key, and so on. The total
-- number of paths is 26 factorial: 4E26, a very big number.

-- We can prune the search tree by realizing that many searches are
-- duplicates. For example, suppose we pick 'a' first, then 'b',
-- then 'c'. This is the same as first picking 'b', then 'a', then
-- 'c'. So if we find ourselves searching at 'c' after having
-- uncovered 'a' and 'b', then we should check to see if we've been
-- at this state before. If we have, we don't have to keep
-- searching. What if the cost of abc is different than bac? In
-- general it will be, but we want the lowest cost. So if we process
-- states lowest-cost first, as they come off a heap, then if we
-- encounter a duplicate state, we can be sure that the first state
-- was better, or at least not worse.

openDoor :: MazeGraph -> Char -> MazeGraph -- aka removeVertex
openDoor g d | d `M.member` g = --traceShow ("open door", d) $ traceShow g $ traceShow g' $ traceShowId $
               M.mapWithKey merge g'
             | otherwise = error $ show ("no such door", d, g)
  where
    paths = --traceShowId $
      g!d
    g' = M.delete d g
    merge pt nbrs | pt == d = error "should not be here"
                  | d `M.member` nbrs = M.unionWith min (M.delete d nbrs) nbrsThruDoor
                  | otherwise =  nbrs
      where nbrsThruDoor = fromList [ (n, cost + doorCost)
                                    | (n, cost) <- toList $ paths
                                     , n /= pt
                                     , n /= d]
            doorCost =  nbrs!d
            
keysReachable :: MazeGraph -> Char -> [(Char, Cost)]
keysReachable g pt
  | pt  `M.member` g
  = --traceShow ("keysReachable", g, pt) $ traceShowId $
    [(nbr, cost) | (nbr, cost) <- M.toList $ g!pt
                 , isLower nbr]
  | otherwise = error "bad pt"

findShortestPath :: MazeGraph -> Set (Cost, [String]) -> (Cost, [Char])
findShortestPath g heap = search (M.singleton "" g) S.empty $ heap
  where search :: Map [Char] MazeGraph -> Set [Char] -> Set (Cost, [Char]) -> (Cost, [Char])
        search gs seen heap
          | null heap = error "null heap" -- should never have an empty heap
          | null g'' = best -- collected all the keys, return
          | (location:keysSoFar) `S.member` seen = search gs seen heap' -- we've been here before, skip
          | otherwise = -- traceShow ("continuing fsp, heap ", heap'') $
            search gs' seen' heap''
          where (best@(cost, path), heap') = -- traceShow ("pulling from heap: gs = ", gs)  $ traceShowId $
                  S.deleteFindMin heap
                keysSoFar = sort $ tail path -- keys so far is the path up to the last node, in sorted (connonical) form
                keysSoFar' = sort path
                location = head path
                seen' = (location: keysSoFar) `S.insert` seen
                g' = gs ! keysSoFar
                g'' = gs' ! keysSoFar' -- graph with whole path removed
                gpartial | isLower location && toUpper location `member` g' = openDoor g' $ toUpper location
                         | otherwise = g'
                gs' | keysSoFar' `member` gs = gs
                    | isLower location = M.insert keysSoFar' (openDoor gpartial $ location)  gs
                    | otherwise = M.insert keysSoFar' (openDoor g' location) gs
                gs''| isLower location = M.insert keysSoFar' (openDoor g' $ toUpper location) gs
                heap'' :: Set (Cost, [Char])
                heap'' = foldl (\h (nbr, nbrcost)-> S.insert (nbrcost+cost, nbr:path) h) heap' $ keysReachable gpartial location


main = do
  -- [instructionFile] <- getArgs
  --mazeString <- readFile "18.input.txt" -- instructionFile
  -- mazeString <- readFile "18.2.input.txt" -- instructionFile
  -- mazeString <- readFile "18.test4" -- instructionFile
  --let maze = strToMaze mazeString
  --let mazeGraph = mazeToGraph maze
  -- print $ mazeToGraph' maze
  --print mazeGraph
  -- let positions = mazeToPositions maze
  --print ("maze size", M.size maze, "graph size", M.size mazeGraph) -- reduced from 3201 nodes to 53 nodes
  --print $ sort $ M.keys mazeGraph 

  -- putStrLn "Part 1"

  -- print $ findShortestPath mazeGraph $ S.singleton (0, ['@'])

  putStrLn "Part 2"
  mazeGraph2 <- readFile "18.2.test1" >>= (return . mazeToGraph . strToMaze)
  --mazeGraph2 <- readFile "18.2.input.txt" >>= (return . mazeToGraph . strToMaze)
  print mazeGraph2
  print $ findShortestPath mazeGraph2 $ S.singleton (0,["1","2","3","4"])
