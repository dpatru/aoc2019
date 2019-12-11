


main = interact (addNewline . show . sum . (map (f . read)) . lines) 
  where f x = floor $ x / 3.0  - 2.0
        addNewline s = s ++ "\n"
