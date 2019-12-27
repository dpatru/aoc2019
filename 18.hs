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
import Data.List (find, intercalate, intersperse, permutations, inits, tails, isPrefixOf, sort, subsequences, nub)
import Data.List.Split (splitOn)
-- import Data.Complex (Complex((:+)), realPart, imagPart) -- define my own complex

import UI.NCurses
import Data.Char (chr, ord, isLower, isUpper, toUpper, isDigit)
-- import Data.Complex (Complex((:+)))
import Data.Maybe (fromJust)


-- removeEach xs =  zip xs (map (flip List.delete xs) xs)


data Complex a = C a a

instance (Show a) => Show (Complex a) where
  show (C x y) = "C " ++ show x ++ " " ++ show y
  
instance (Eq a) => Eq (Complex a) where
  (C a ai) == (C b bi) = a == b && ai == bi
  
instance (Eq a, Ord a) => Ord (Complex a) where
  compare (C a ai) (C b bi) | a < b || a == b && ai < bi = LT
                            | a > b || a == b && ai > bi = GT
                            | otherwise = EQ

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

openDoors :: MazeGraph -> [Char] -> MazeGraph -- aka removeVertex
openDoors g ds = foldl openDoor g ds

openDoor :: MazeGraph -> Char -> MazeGraph -- aka removeVertex
openDoor g d | d `M.member` g = --traceShow ("open door", d) $ traceShow g $ traceShow g' $ traceShowId $
               M.mapWithKey merge g'
             | otherwise = -- trace ("openDoor: door "++ show d ++ "not in graph ") $
               g -- error $ show ("no such door", d, g)
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
            
-- keysReachable :: MazeGraph -> Char -> [(Char, Cost)]
-- keysReachable g pt
--   | pt  `M.member` g
--   = --traceShow ("keysReachable", g, pt) $ traceShowId $
--     [(nbr, cost) | (nbr, cost) <- M.toList $ g!pt
--                  , isLower nbr]
--   | otherwise = error "bad pt"

isCapitalized s = isUpper $ head s
isLowered s = isLower $ head s
capitalize s = (toUpper $ head s): tail s

findShortestPath :: (String -> Bool) -> MazeGraph -> Set (Cost, String) -> (Cost, [Char])
findShortestPath isDone graph myheap = search (M.singleton "" graph) S.empty $ myheap


  -- 'search graphs isDone seen heap' removes the best state (state with
  -- lowest cost) from the heap and checks if we're done. If we're
  -- not, then it inserts the next states on the heap and recurses.

  -- graphs is a dictionary which holds graphs with various states
  -- removed. graphs!"abc" is the graph with states a, b, and c
  -- removed. Note that "abc" is sorted.

  -- The heap stores states in the tuple (363, "abc1 xyz2 uv3 4")
  -- where the first element is the cost and the send is the paths
  -- concatenated with a space. The initial state is (0, "1 2 3 4")

  -- I use the convention of adding a tick to denote the next
  -- variable, so gs' is the next gs.
  where search ::  Map [Char] MazeGraph -> Set [Char] -> Set (Cost, String) -> (Cost, [Char])
        search gs seen heap
          | null heap = error "null heap" -- should never have an empty heap
          | isDone pathsString
          = traceShow "Done" $
            best -- collected all the keys, return
          | pathsString `S.member` seen
          = traceShow ("Search seen: ", cost, pathsString, "new heapsize", S.size heap') $
            search gs seen heap' -- we've already seen this state
          | otherwise
          = traceShow ("Search running: ", cost, best, pathsString, "new heapsize", S.size heap'') $ trace "\n" $
            -- traceShow ("new heap", S.toList heap'' ) $
            search gs' seen' heap''
          where
            (best@(cost, pathsString), heap') = S.deleteFindMin heap
            seen' = pathsString `S.insert` seen
            paths = [ p | p <- words pathsString, not $ null p ]
            state0 = sort $ xs ++ cxs
              where xs = concatMap tail paths
                    cxs = map toUpper $ Prelude.filter isLower xs
            g0 | state0 `M.member` gs = gs!state0
               | otherwise = error $  "bad state0 " ++ state0 ++ ": keys for gs are: " ++ (show $ M.keys gs)++ "\nbest = " ++ show best ++"\nstateNow = " ++ stateNow
            stateDelta = -- trace "stateDelta" $ traceShowId $
              concatMap (map toUpper . Prelude.filter isLower) paths
            stateNow = sort $ state0 ++ stateDelta
            gNow = openDoors g0 stateDelta -- This is the current graph with the doors open.


            -- gNext is a list of the next graphs, once any path is
            -- lengthened. When a path is lengthened, the old pathhead
            -- is removed from the graph. It's only needed so that
            -- neighbors can be calculated. After that, it just slows
            -- up the search.
            gNext = S.fromList $ 
                    [(sort $ h:stateNow, openDoor gNow h)
                    | h <- map head paths ]
                    ++ [(sort $ h'++state0, openDoors g0 h') | h <- map head paths, h' <- if h == toUpper h then [[h]] else [[h], [h, toUpper h]]]
            gs' = foldr (uncurry M.insert) gs $ (stateNow, gNow): S.toList gNext
            newStates = [(cost+nbrCost, pathsString')
                        | i <- [0 .. (length paths - 1)]
                        , let (paths0, (x: xs): paths2) = splitAt i paths -- expand from x
                        , (nbr, nbrCost) <- M.toList $ gNow!x
                        , isLower nbr
                        , let pathsString' = unwords $ paths0 ++ (nbr:x:xs): paths2
                        ]
            heap'' = foldr S.insert heap' newStates


main = do
  -- [instructionFile] <- getArgs
  -- putStrLn "Part 1"
  -- mazeString <- readFile "18.input.txt" -- instructionFile
  -- -- mazeString <- readFile "18.2.input.txt" -- instructionFile
  -- -- mazeString <- readFile "18.test4" -- instructionFile
  -- let maze = strToMaze mazeString
  -- let mazeGraph = mazeToGraph maze
  -- -- print $ mazeToGraph' maze
  -- -- print mazeGraph
  -- let positions = mazeToPositions maze
  -- print ("maze size", M.size maze, "graph size", M.size mazeGraph) -- reduced from 3201 nodes to 53 nodes
  -- print $ sort $ M.keys mazeGraph 
  -- print $ findShortestPath isDone mazeGraph $ S.singleton (0, ['@'])

  putStrLn "Part 2"
  let file2 = "18.2.input.txt"
  -- let file2 = "18.2.test1"
  -- mazeGraph2 <- readFile "18.2.test1" >>= (return . mazeToGraph . strToMaze)
  mazeGraph2 <- readFile file2 >>= (return . mazeToGraph . strToMaze)
  let allKeys = Prelude.filter isLower $ M.keys mazeGraph2
  let isDone state = all (`elem` state') allKeys
        where state' = Prelude.filter isLower state
        
  -- print mazeGraph2
  print $ findShortestPath isDone mazeGraph2 $ S.singleton (0, "1 2 3 4")
