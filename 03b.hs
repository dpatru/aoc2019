import Data.Map.Strict (empty, member, fromList, (!))
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)

type Distance = Int
type Coord = (Int, Int)
type Direction = String
type Directions = [Direction]

directionsToCoords :: Coord -> Directions -> [Coord]
directionsToCoords start directions = reverse $ foldl f [start] $ map parse directions
  where f :: [Coord] -> (Char, Distance) -> [Coord]
        f ((x,y): xys) (dir, dist) = case dir of
          'U' -> reverse [(x, y+i) | i <- [1 .. dist]] ++ ((x,y) : xys)
          'D' -> reverse [(x, y-i) | i <- [1 .. dist]] ++ ((x,y) : xys)
          'R' -> reverse [(x+i, y) | i <- [1 .. dist]] ++ ((x,y) : xys)
          'L' -> reverse [(x-i, y) | i <- [1 .. dist]] ++ ((x,y) : xys)
          _ -> error "bad direction"
        parse :: Direction -> (Char, Distance)
        parse s = (head s, read $ tail s)

addSteps :: [Coord] -> [(Coord, Int)]
addSteps cs = zip cs [0 ..]

process :: [Directions] -> Maybe(Coord, Distance)
process [l1, l2] = foldl test Nothing $ addSteps $ directionsToCoords (0,0) l2
  where test :: Maybe (Coord, Int) -> (Coord, Int) -> Maybe (Coord, Int)
        test Nothing  (c, s) | member c path1 && c /= (0,0) = Just (c, path1!c + s)
                        | otherwise = Nothing
        test (Just (bestc, bestd)) (c, s) | member c path1 && c /= (0,0) && path1!c + s < bestd = Just (c, path1!c + s)
                                   | otherwise = Just (bestc, bestd)
        path1 = fromList $ addSteps $ directionsToCoords (0,0) l1
process _ = error "too many lines"

main1 = interact (newLn . show . fromJust . process . map (splitOn ",") . lines)
  where newLn s = s ++ "\n"

main2 = interact (newLn . show . map (directionsToCoords (0,0)) . map (splitOn ",") . lines)
  where newLn s = s ++ "\n"

main = main1
