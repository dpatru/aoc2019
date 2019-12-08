import Data.Map.Strict (Map, fromList, foldl', foldrWithKey', (!), insert, member, empty)
import Data.List.Split (splitOn)
import Data.Tuple (swap)

main = interact $ show . counts empty . parse . lines
  where parse :: [String] -> Map String String
        parse orbits = fromList $ map (myswap . splitOn ")") orbits
        myswap :: [String] -> (String, String)
        myswap [a,b] = (b,a)
        myswap _ = error "bad split"
        counts :: Map String Int -> Map String String -> Int
        counts c p = foldl' (+) 0 $ fst $ foldrWithKey' count (c,p) p
        count :: String -> String -> (Map String Int, Map String String) -> (Map String Int, Map String String)
        count orbiter orbitee (orbits, orbitees)
          | orbiter `member` orbits = (orbits, orbitees) -- skip if already handled
          | orbitee `member` orbits = (insert orbiter (orbits!orbitee + 1) orbits, orbitees) -- add orbiter to orbits
          | orbitee `member` orbitees = count orbiter orbitee $ count orbitee (orbitees!orbitee) (orbits, orbitees)
          | otherwise = (insert orbitee 0 (insert orbiter 1 orbits), orbitees)
        

