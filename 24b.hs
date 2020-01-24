import Debug.Trace
import System.IO (readFile)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict as M 
import Data.Set as S
import Data.Set (Set)

type Point = (Int,Int,Int)
type Bugs = Set Point

coords = [(x,y) | x <- [0 .. 4]
                , y <- [0 .. 4]
                , (x,y) /= (2,2)]

neighbors (x,y,z)
  | (x,y) == (2,2)
  = error "bad middle coordinate"
  
  | x < 0 || x > 4 || y < 0 || y > 4
  = error "coordinates over the edge"
  
  | (x,y) == (1,2)
  = [(0,2,z),(1,1,z),(1,3,z)]++[(0,j,z+1)|j<-[0..4]]
  
  | (x,y) == (2,1)
  = [(2,0,z),(1,1,z),(3,1,z)]++[(j,0,z+1)|j<-[0..4]]
  
  | (x,y) == (2,3)
  = [(2,4,z),(1,3,z),(3,3,z)]++[(j,4,z+1)|j<-[0..4]]
  
  | (x,y) == (3,2)
  = [(4,2,z),(3,1,z),(3,3,z)]++[(4,j,z+1)|j<-[0..4]]

  | otherwise
  = Prelude.map adjust [(x,y-1),(x,y+1),(x-1,y),(x+1,y)]
  where adjust (x,y)
          | x < 0 = (1,2,z-1)
          | x > 4 = (3,2,z-1)
          | y < 0 = (2,1,z-1)
          | y > 4 = (2,3,z-1)
          | (x,y) == (2,2) = error "bad neighbor"
          | otherwise = (x,y,z)

adjacent pt b = sum [1 | n <- neighbors pt
                       , n `S.member` b]

infest :: Bugs -> Bugs
infest b = S.foldr f S.empty $ S.fromList $ concatMap neighbors $ S.toList b
  where f p b' = let n = adjacent p b
                 in if n == 1 || (not $ S.member p b) && n == 2 then S.insert p b' else b'

main = do
  bugs_in <- readFile "24.input.txt"
  let bugs = S.fromList [(x,y,0) | (y, l) <- zip [0 ..] $ lines bugs_in
                                 , (x, c) <- zip [0 ..] l
                                 , c == '#'
                                 ]
  print bugs
  print $ S.size $ (iterate infest bugs)!!200
  
