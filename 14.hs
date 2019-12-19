import Data.Map.Strict

main = do
  [file] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings
