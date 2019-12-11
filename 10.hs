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

import Data.List (sort, nub)


readMap :: [[Char]] -> [(Int, Int)]
readMap rows = [(x, y) | (y, row) <- zip [0 ..] rows
                       , (x, c) <- zip [0 ..] row
                       , c == '#']

analyzeMap :: [(Int, Int)] -> (Int, (Int, Int))
analyzeMap points = maximum [(length $ nub angles, p)
                            | p <- points
                            , let angles = map (fst . polarDirection p) points]
                                                 
polarDirection :: (Int, Int) -> (Int, Int) -> (Float, Float)
polarDirection origin@(x0,y0) pt@(x1,y1) = -- (angle_from_vertical, distance)
  swap -- angle first
  $ positiveAngle -- polar's angle is from -pi to pi, convert to 0 to 2pi so that we can sort by angle
  $ polar -- convert to (magnitude, phase)
  $ fromIntegral (-1 * y) :+ fromIntegral x -- make complex, normally
           -- the positive x axis is phase 0, but here we want to go
           -- clockwise from vertical, so swap x and y and change y's
           -- sign to correct for the fact that the coordinates
           -- increase in the down direction.
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
        $ filter (/= station) points
        
  putStrLn $ "Part 2: " ++ show (polarPoints!!199)
  --putStrLn $ "Part 2: " ++ (show $ head $ drop 199 polarPoints)
  



