


main = interact (addNewline . show . sum . (map (f . read)) . lines) 
  where f x = if y > 0 then y + f (fromIntegral y) else 0
          where y :: Int
                y = floor $ x / 3  - 2.0
        addNewline s = s ++ "\n"
