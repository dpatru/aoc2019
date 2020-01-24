import Debug.Trace
import System.IO (readFile)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict as M 
import Data.Set as S
import Data.Set (Set)

type Bugs = Set (Int,Int)

coords = [(x,y) | x <- [0 .. 4]
                , y <- [0 .. 4]]

neighbors (x,y) = [(i,j)
                  | i <- [x-1 .. x+1]
                  , j <- [y-1 .. y+1]
                  , (i,j) /= (x,y)
                  , i >= 0
                  , j >= 0
                  , i < 5
                  , j < 5
                  , i == x || j == y]

adjacent (x,y) b = sum [1 | n <- neighbors (x,y)
                          , n `S.member` b]

infest :: Bugs -> Bugs
infest b = S.fromList [p | p <- coords
                         , let n = adjacent p b
                         , p `S.member` b && n == 1
                           || (not $ S.member p b) && (n == 1 || n == 2)
                         ]

firstRepeat :: Set Bugs -> Bugs -> Bugs
firstRepeat seen b = if b `S.member` seen then b else firstRepeat (S.insert b seen) (infest b)

biodiversity b = sum [2^(x+5*y) | (x,y) <- coords, (x,y) `S.member` b]

main = do
  bugs_in <- readFile "24.input.txt"
  let bugs = S.fromList [(x,y) | (y, l) <- zip [0 ..] $ lines bugs_in
                               , (x, c) <- zip [0 ..] l
                               , c == '#'
                               ]
  print bugs
  print $ biodiversity $ firstRepeat S.empty bugs
  
