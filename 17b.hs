import Data.List (inits)


substrings :: [a] -> [[a]]
substrings [] = []
substrings xs = inits xs ++ (substrings $ tail xs)


main = do
  let dsubs = substrings "daniel"
  print $ length dsubs
  let subs72 = substrings [1 .. 72]
  print $ length subs72