import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.Tuple (swap)
import Data.Complex (Complex((:+)), polar)

import Data.List (sort)

-- gcd :: Int -> Int -> Int -- already part of prelude
-- gcd a b = if x == 0 then abs $ a else gcd x a
--   where x = b `mod` a

reduce :: (Int, Int) -> (Int, Int)
reduce (a,b) | a == 0 && b == 0 = error "same point"
             | a == 0 = (0, signum b)
             | b == 0 = (signum a, 0)
             | otherwise = (a `div` x, b `div` x)
  where x = abs $ gcd a b

direction :: (Int, Int) -> (Int, Int) -> (Int, Int)
direction (a,b) (c,d) = reduce (a-c, b-d)

readMap :: [[Char]] -> Set (Int, Int)
readMap rows = S.fromList $
  [(x, y) | (y, row) <- zip [0 ..] rows
          , (x, c) <- zip [0 ..] row
          , c == '#']

analyzeMap :: Set (Int, Int) -> (Int, (Int, Int))
analyzeMap points = maximum $ map swap $ M.toList  $ M.map S.size $ S.foldr' insertPt M.empty points
  where insertPt :: (Int, Int) -> Map (Int, Int) (Set(Int, Int)) -> Map (Int, Int) (Set(Int, Int))
        insertPt p m = M.insert p (analyzePoint p) m
        analyzePoint :: (Int, Int) -> Set(Int,Int)
        analyzePoint p = -- trace (show p) $
          S.foldr' test S.empty points
          where test p2 m | p == p2 = m
                          | d `S.member` m = m
                          | otherwise = S.insert d m
                  where d = direction p p2

polarDirection :: (Int, Int) -> (Int, Int) -> (Float, Float)
polarDirection origin@(x0,y0) pt@(x1,y1) = -- (angle_from_vertical, distance)
  swap -- angle first
  $ positiveAngle
  $ polar -- convert to (magnitude, phase)
  $ fromIntegral (-1 * y) :+ fromIntegral x -- make complex normally the positive x axis is phase 0,
           -- but here we want to go clockwise from vertical, so swap
           -- x and y
  where (x, y) = (x1-x0,y1-y0) -- point from perspective of origin
        positiveAngle (m,p) | p < 0 = (m, 2 * pi + p)
                            | otherwise = (m, p)

main = do
  [mapFile] <- getArgs
  m <- readFile mapFile
  let points = readMap $ lines m
  let (reachable, station) = analyzeMap points
  putStrLn $ "Part 1: " ++ show (reachable, station)
  let polarPoints = -- traceShowId $ 
        sort -- [(round, angle, xy)] in order
        $ foldl (\l ((angle, distance), xy) ->
                   (if null l then 0
                    else let (round0, angle0, pt0) = head l
                         in if angle == angle0 then round0 + 1
                            else 0,
                     angle, xy):l) ([]::[(Int, Float, Int)])
        -- $ traceShowId
        $ sort -- [((angle, distance), xy)] in order
        $ map (\(x,y) -> (polarDirection station (x,y),100*x+y))
        $ S.toList
        $ S.delete station points
        
  putStrLn $ "Part 2: " ++ show (polarPoints!!199)
  



