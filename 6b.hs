import Data.Map.Strict (Map, fromList, foldl', foldrWithKey', (!), insert, member, empty)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

main = interact $ show . intersect "SAN" 0 . build "YOU" 0 empty . parse . lines
  where parse :: [String] -> Map String String -- generates map of orbitees (from orbiters to orbitees)
        parse orbits = fromList $ map (myswap . splitOn ")") orbits
        myswap :: [String] -> (String, String)
        myswap [a,b] = (b,a)
        myswap _ = error "bad split"
        build s steps path orbiters
          | s `member` orbiters
          = let s2 = orbiters!s in
              build s2 (steps+1) (insert s2 steps path) orbiters
          | otherwise
          = (path, steps, orbiters)
        intersect s steps (path1, len1, orbiters)
          | s `member` orbiters
          = let s2 = orbiters!s in
              if s2 `member` path1
              then steps + path1!s2
              else intersect s2 (steps+1) (path1, len1, orbiters)
          | otherwise
          = steps + len1
        

