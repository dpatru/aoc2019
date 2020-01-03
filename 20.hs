
import Debug.Trace
import System.IO (readFile)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (sort, intercalate)
import Data.Maybe (fromMaybe, fromJust, isNothing)
-- import Data.Char (Char)

type Point = (Int, Int)
type Point2 = (Point, Int) -- x,y,level
type Maze = Map Point String
type Graph = Map Point (Map Point Int)

neighbors :: Point -> [Point]
neighbors (x,y) = [(i,j) | (i,j) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
                         , i >= 0 && j >= 0]
neighborsInMaze m pt = filter (`M.member` m) $ neighbors pt

firstAndOnly :: Show a => [a] -> a
firstAndOnly xs | length xs == 1 = head xs
                | otherwise = error $ "expecting a list with just one item, got: " ++ show xs

nameToPoint :: Maze -> String -> Point
nameToPoint maze = \n -> m!n
  where m :: Map String Point
        m = M.fromList [(n, pt) | (pt, n) <- M.toList maze]
  
linesToMaze :: [String] -> Maze
linesToMaze ls = labels `M.union` spaces
 where pts :: Map Point Char
       pts = M.fromList [ ((x,y), c)
                        | (l, y) <- zip ls [0 ..]
                        , (c, x) <- zip l [0 ..]
                        , c /= '#', c /= ' ']
       spaces :: Map Point String
       spaces = M.map return $ M.filter (== '.') pts
       chars :: Map Point Char
       chars = M.filter (/= '.') pts
       labels :: Map Point String
       labels = M.fromList $
         [(pt', label) | (pt, c) <- M.toList chars
                       , let nbrs = neighborsInMaze spaces pt
                       , length nbrs == 1 -- we're looking for the
                                          -- label character that is
                                          -- between the maze space
                                          -- and the other label
                                          -- character.
                       , let pt' = firstAndOnly nbrs
                       , let pt2 = firstAndOnly $ neighborsInMaze chars pt
                       , let c2 = chars ! pt2
                       , let (_, label) = unzip $ sort $ [(pt2, c2), (pt, c)]
                       ]

type Cost = Int

pointToName :: Maze -> Point -> String
pointToName m pt = (m!pt) -- ++ show pt

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = concat [[(x,x2),(x2,x)] | x2 <- xs] ++ allPairs xs

pair :: a -> b -> (a,b)
pair a b = (a,b)

mazeToGraph :: Maze -> Graph
mazeToGraph m = buildGraph M.empty m S.empty heap0
  where heap0 = S.fromList $ walks0 ++ jumps0
        labels = [(pt, label) | (pt, label) <- M.toList m, length label > 1]
        walks0 = [(0, (pt, pt)) | (pt, label) <- labels]
        jumps0 = [(1, ps) | ps <- concatMap allPairs $ M.elems $
          M.fromListWith (++) [(label, [pt]) | (pt, label) <- labels]]
        buildGraph :: Graph -> Maze -> Set (Point, Point) -> Set (Cost, (Point, Point)) -> Graph
        buildGraph g m seen heap
          | null heap = g -- done
          | (pathStart, pathStop) `S.member` seen -- skip this, we've seen it before
          = buildGraph g m seen heap' 
          | pathStop == pathStart || (length $ m!pathStop) < 2 -- keep going
          = buildGraph g m seen' heap''
          | otherwise 
          = buildGraph g' m seen' heap' -- we've found a shortest path to another labeled node, put it into the graph and continue.
          where ((cost, (pathStart, pathStop)), heap') = S.deleteFindMin heap
                seen' = S.insert (pathStart, pathStop) seen
                heap'' = foldr S.insert heap $
                  [(cost+1, (pathStart, n)) | n <- neighborsInMaze m pathStop]
                g' = M.insertWith (M.unionWith min) pathStart (M.singleton pathStop cost) g

searchGraph :: Graph -> Point -> Point -> (Cost, [Point])
searchGraph g start stop = search S.empty $ S.singleton (0, [start])
  where search seen heap
          | null heap = error "Couldn't get to destination"
          | pathStop == stop
          = best
          | pathStop `S.member` seen
          = search seen heap'
          | otherwise
          = search seen' heap''
          where (best@(cost, pathStops@(pathStop:_)), heap') = S.deleteFindMin heap
                seen' = S.insert pathStop seen
                heap'' = foldr S.insert heap' $
                  [(cost+cost', pathStop':pathStops)
                  | (pathStop', cost') <- M.toList $ g!pathStop
                  , not $ pathStop' `S.member` seen' -- I think this is redundant
                  ]

-- Part 2
-- To handle recursive graphs, we just need to acknowledge levels and modify the graph transitions to change levels.

type Graph2 = Point2 -> [(Point2, Cost)]

gToG2 :: Maze -> Graph -> (Point -> Bool) -> [Point] -> [Point] -> Graph2
gToG2 m g isEntryOrExit downGates upGates (pt, level)
  = -- trace ("gToG2: " ++ showPoint2 m (pt, level)) $ traceShowId $
    [ ((pt', level'), cost)
    | (pt', cost) <- --traceShowId $
      M.toList $ g!pt
    , let level' | m!pt /= m!pt' = level
                 | pt == pt' = error "loop"
                 | pt `elem` downGates = level - 1
                 | pt `elem` upGates = level + 1
                 | otherwise = error "same label but not in up or down"
    , level' >= 0 -- don't allow negative levels
    , level' == 0 || (not $ isEntryOrExit pt') -- don't allow entry or exit from non-zero level
    ]

searchGraph2 :: Maze -> Graph2 -> Point -> Point -> (Cost, [Point2])
searchGraph2 m g start stop = search S.empty $ S.singleton (0, [(start, 0)])
  where
    search :: Set Point2 -> Set (Cost, [Point2]) -> (Cost, [Point2])
    search seen heap
      | null heap = error "Empty heap. Couldn't get to destination"
      | fst pathStop == stop
      = best
      | pathStop `S.member` seen
      = -- trace ("\nsearchGraph2 skipping state in seen "++ showPath2 m best ++", heap size now is "++(show $ S.size heap')++"\n") $
        search seen heap'
      | otherwise
      = -- trace ("\nsearchGraph2 on state "++ showPath2 m best ++", heap size now is "++(show $ S.size heap'')++"\n") $
        search seen' heap''
      where
        (best@(cost, pathStops@(pathStop:_)), heap') = S.deleteFindMin heap
        seen' = S.insert pathStop seen
        heap'' = foldr S.insert heap' $
                 [(cost+cost', pathStop':pathStops)
                 | (pathStop', cost') <- g pathStop
                 -- , not $ fst pathStop' `S.member` seen' -- I think this is redundant
                 ]

showPoint :: Maze -> Point -> String
showPoint m pt = m!pt ++ show pt

showPoints m pts = intercalate ", " $  map (showPoint m) pts
showPointsSorted m pts = intercalate ", " $  sort $ map (showPoint m) pts

showPoint2 :: Maze -> Point2 -> String
showPoint2 m (pt, level) = "("++showPoint m pt++", "++show level++")"

showPath2 :: Maze -> (Cost, [Point2]) -> String
showPath2 m (c,pth) = "(" ++ show c ++ ", " ++ pth' ++ ")"
  where pth' =  concatMap show [(m!pt, level) | (pt, level) <- pth]
        -- pth' =  "path length " ++ (show $ length pth)
        
traceShowF f x = traceShow (f x) x

main = do
  putStrLn "Part 1"
  -- maze <- readFile "20.2.test1" >>= return . linesToMaze . lines
  maze <- readFile "20.input.txt" >>= return . linesToMaze . lines
  -- maze <- readFile "20.test1" >>= return . linesToMaze . lines
  let g = mazeToGraph maze
  print $ M.size g
  print $ g
  let nToP = nameToPoint maze
      aa = nToP "AA"
      zz = nToP "ZZ"
      (cost, path) = searchGraph g aa zz
  print (cost, map (maze!) path)

  putStrLn "Part 2"
  let points = M.keys g
      xs = map fst points
      ys = map snd points
      xMin = minimum xs
      xMax = maximum xs
      yMin = minimum ys
      yMax = maximum ys
      isOutside (x,y) = x `elem` [xMin,xMax] || y `elem` [yMin,yMax]
      updownGates = filter (\pt -> pt /= aa && pt /= zz) points 
      downGates = trace "downGates" $ traceShowF (showPointsSorted maze) $
        filter isOutside updownGates
      upGates = trace "upGates" $ traceShowF (showPointsSorted maze) $
        filter (not . isOutside) updownGates
      isEntryOrExit pt = pt `elem` [aa, zz]
      g2 = gToG2 maze g isEntryOrExit downGates upGates
      (cost, path) = searchGraph2 maze g2 aa zz
  print (cost, [(maze!pt, level) | (pt, level) <- path])
  
