import System.Environment (getArgs)
import System.IO (readFile)
import Data.List.Split (splitOn)
import Data.Map.Strict (empty, Map, (!), insert, insertWith, findWithDefault, delete, singleton)
import qualified Data.Map.Strict as M
import Data.Char (toLower)

type Reactions = Map String [(String, Int)]
-- Chemical key produced in quantity X by combination of chemicals in
-- certain quantities.

trimLeft :: String -> String
trimLeft s = dropWhile (== ' ') s

trim :: String -> String
trim s = trimLeft $ reverse $ trimLeft $ reverse s

readReactions :: [String] -> Reactions
readReactions rs = foldl addReaction empty $ map trim rs
  where addReaction :: Reactions -> String -> Reactions
        addReaction m rxn = insert el inputs' m
          where [inputs, output] = splitOn " => " rxn
                [q, el] = words output
                inputs' = [(c, read q) | input <- splitOn ", " $ "-"++output++", "++inputs
                                       , let [q, c] = words input]

type Tracker = ([String], Map String Int) -- elements to reduce, quantities, negative quantity means extra

-- makeOreMax :: Map String a -> Map String a
-- makeOreMax m = delete "ORE" $ insert "ore" (m!"ORE") m

reduceFuel :: Reactions -> Tracker -> Int
reduceFuel r ([], quantities) = quantities!"ORE"
reduceFuel r ((el:els), quantities)
  | quantities!el <= 0 = reduceFuel r (els, quantities) -- el is already reduced
  | otherwise = reduceFuel r (els', quantities')
  where els' = els ++ (filter (/= "ORE") $ map fst inputs)
        quantities' = foldl (\qs (i,q)-> insertWith (+) i q qs) quantities inputs'
        inputs = r!el
        q = fromIntegral $ snd $ head $ filter (\(el, q) -> q < 0) inputs
        f = -1 * ((quantities!el) `div` q) -- div truncates to negative infinity, rounding up
        inputs' = [(el, q * f) | (el, q) <- inputs]

newton :: Int -> (Int -> Int) -> Int -> Int -> Int -> Int -> ((Int, Int), (Int, Int))
newton goal f guess0 y0 guess1 y1
  | abs(y0 - y1) <= 1 || abs(guess0 - guess1) <= 1 = ((guess0, y0),(guess1, y1))
  | otherwise = newton goal f guess1 y1 guess2 $ f guess2
  where guess2 = guess1 + round ((fromIntegral $ goal - y1) / slope)
        slope = (fromIntegral $ y1-y0) / (fromIntegral $ guess1-guess0)
        
main = do
  [reactionsFile] <- getArgs
  reactionsStrings <- readFile reactionsFile
  let reactions = readReactions $ lines reactionsStrings
  putStrLn $ show reactions
  let orePerFuel = reduceFuel reactions (["FUEL"], singleton "FUEL" 1)
  putStrLn "Part 1"
  putStrLn $ show orePerFuel
  putStrLn "Part 2"
  putStrLn $ "Naive answer: " ++ (show $ (floor 1e12) `div` orePerFuel)
  let fuelToOre f = reduceFuel reactions (["FUEL"], singleton "FUEL" f)
  putStrLn $ show $ newton (floor 1e12) fuelToOre 0 0 2 $ fuelToOre 2
  
