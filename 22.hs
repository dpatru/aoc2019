{-# LANGUAGE DataKinds, TypeOperators #-}
import Data.List (sort, elemIndex)
import Debug.Trace
import System.IO (readFile)
import Data.Modular

type Cards = [Int]

n = 10007
cards0 = [0 .. (n-1)]

newStack cs = reverse cs

cut x cs = bs ++ as
  where x' | x >= 0 = x
           | otherwise = length cs + x
        (as, bs) = splitAt x' cs

increment x cs = map snd $ sort $ [(i `mod` n, c) | (i, c) <- zip [0, x ..] cs]
  where n = length cs

strToF :: String -> (Cards->Cards)
strToF s | n == 4 && ws!!3 == "stack" = newStack
         | n == 2 = cut (read $ ws!!1)
         | otherwise = increment (read $ ws!!3)
  where ws = words s
        n = length ws

-- for part 2, we can't actually do the shuffling. We'll simply trace
-- the position of the interesting card through each operation.

-- The trick here is to recognize that each of the shuffles can be
-- represented as a function that takes a card x to another card
-- ax+b`mod`deckSize where a and b are numbers.

-- newStack: -x`mod`deckSize For deck = [0 .. 9], newStack deck =
-- [9,8 .. 0], any card x (say 3) is mapped to -x, (3 maps to
-- -3`mod`10 = 7

-- cut a: x-a`mod`deckSize.  For deck = [0 .. 9], cut 3 deck = [3
-- .. 9]++[0 .. 2], any card x (say 2) is mapped to x-3, (3 maps to
-- -1`mod`10 = 9).  7 maps to 7-3 = 4.  cut -7 deck = cut (10-7) deck
-- = cut 3 deck. 2 maps to 2-(-7) = 9. 7 maps to 7-(-7) = 14 `mod`10 =
-- 4.

-- increment a: ax`mod`deckSize.  For deck = [0 .. 9], increment 3
-- deck = moves every card from position i to 3i`mod`10. 6 moves to
-- 18`mod`10 = 8.

-- Now notice that running the card (ax+b) through such a function
-- also produces a card of the same form: a'(ax+b)+b' =
-- (a'a)x+(a'b+b). This means that we can figure out how shuffle
-- operations compose by just tracking how a and b change. For
-- example, cut 3 (x-3) followed by increment 3 (3x) is increment 3
-- applied to the result of cut 3: 3(x-3) = 9x-9 `mod`deckSize. For
-- deck [0..9], 2 will go to -1 after cut 3 and then -1 will go to
-- -3`mod` 10 = 7 (9*3=27`mod`10) after increment 3. This is the same
-- as the fomula for the composition: 3(2-3) = -3.

-- Now part 1 of the problem asks what was the position of card
-- 2019. But part 2 asks what number is on the card that ends up in
-- position 2020? Part 2 therefore is asking us to trace the shuffling
-- in reverse. Starting with the card at 2020 in the shuffled duck,
-- unshuffle the cards step by step until we reach the original deck
-- and report where the card that ended up at 2020 started. So we need
-- to find the inverse operations for each of the shuffle
-- opperations. A newStack is just reversing the the deck. To get back
-- to the original deck, just reverse it again. So, marking the
-- inverse with a tick, newStack' = newStack. Similarly in cut we
-- subtract: cut a x = x-a). To get back to the original deck, we add:
-- cut' a x = x+a. Finally increment is a multiply, to get back the
-- original deck, multiply by the inverse. increment' a x = x * inv a.

-- But, given the large deck size and the large number of shuffles, we
-- have to further analyze. The critical insight is to see that each
-- shuffle is just a linear transformation of the form ax +
-- b. newStack, or reverse, is just -x. Cut b is -b. And increment a
-- is just ax. Notice also that applying multiple linear
-- transformations gives another linear transformation. If f = ax+b
-- and g = a'x+b', then f.g = a(a'x+b')+b = aa'x+(ab'+b). This
-- suggests that we can shuffle merely be keeping track of the a pair,
-- (a,b). The to do multiple shuffles, we can just multiply a
-- transformation by itself repeatedly.

type Shuffle = (Integer, Integer) -- (a,b) => ax+b 
compose :: Integer -> Shuffle -> Shuffle -> Shuffle
compose m (a,b) (c,d) = ((a*c) `mod` m, (a*d+b) `mod` m)

shuffle :: String -> Integer -> Shuffle -> Shuffle
shuffle instructions deckSize (a,b) = foldl shuffle1 (a,b) $ lines instructions
  where shuffle1 :: Shuffle -> String -> Shuffle
        shuffle1 (a,b) s
          | n == 4 && ws!!3 == "stack"  -- newStack (reverse)
          = -- trace "newStack" $ traceShowId $
            compose deckSize (-1, -1) (a,b)  
          | n == 2   -- cut (read $ ws!!1)
          = -- trace "cut" $ traceShowId $
            compose deckSize (1, -1 * arg) (a,b) 
          | otherwise  -- increment (read $ ws!!3)
          = -- trace "increment" $ traceShowId $
            compose deckSize (arg, 0) (a,b) 
          where ws = words s
                n = length ws
                arg = read $ ws !! (n-1)

shuffles :: Integer -> Integer -> Shuffle -> Shuffle
shuffles deckSize n (a,b)
  | n == 0 = -- trace "shuffles base case" $ traceShowId $
    (1,0)
  | r == 1 = -- trace "shuffles odd" $ traceShowId $
    compose deckSize (a,b) shuffles'
  | otherwise = -- trace "shuffles even" $ traceShowId $
    shuffles'
  where (q,r) = divMod n 2
        shuffles' = shuffles deckSize q $ compose deckSize (a,b) (a,b)

-- Once we have the complete shuffle (a,b), we need to invert it.
-- finalPosition = a*initialPosition+b, given final position,
-- find initial position.
-- initialPosition = (finalPosition - b) * inv a

-- Extended euclidian algorithm
-- given integers a and b, find s and t such that sa + tb = gcd(a,b)
eea :: (Show a, Integral a) => a -> a -> (a, a, a)
eea a b | r == 0 =
          (0,1,b)
        | (s,t) == (0,1) = 
          (t, s-q, d)
        | otherwise = 
          (t, (s- q*t), d)
  where (q,r) = divMod a b
        (s,t,d) = eea b r

modularMultaplicativeInverse a m = s
  where (s, t, d) = eea a m

invert :: Integer -> Shuffle -> Integer -> Integer
invert deckSize (a,b) finalPosition
  = ((finalPosition - b) * modularMultaplicativeInverse a deckSize) `mod` deckSize
  
-- newstack' deckSize card = deckSize - 1 - card

-- cut' deckSize x card | x < 0 = cut' deckSize (deckSize + x) card
--                      | card >= x = card - x
--                      | otherwise = deckSize + card - x


-- increment' deckSize x card = (card * modularMultaplicativeInverse x deckSize) `mod` deckSize

-- inverse' :: String -> Integer -> Integer -> Integer
-- inverse' instructions deckSize card0 = foldl inverse1 card0 $ reverse $ lines instructions
--   where inverse1 :: Integer -> String -> Integer
--         inverse1 card s
--           | n == 4 && ws!!3 == "stack"  -- newStack (reverse)
--           =  newStack' deckSize card
--           | n == 2   -- cut (read $ ws!!1)
--           = cut' deckSize (deckSize - (read $ ws !! 1)) card
--           | otherwise  -- increment (read $ ws!!3)
--           = increment' deckSize (read $ ws !! 3) card
--           where ws = words s
--                 n = length ws
  
main = do
  -- putStrLn "Test"
  -- let xs :: [Int]
  --     xs = [0 .. 9]
  -- print $ ("newStack", newStack xs)
  -- print $ ("cut 3", cut 3 xs)
  -- print $ ("cut -4", cut (-4) xs)
  -- print $ ("increment 3", increment 3 xs)

  instructionString <- readFile "22.input.txt"
  -- putStrLn "Part 1"
  -- let cards = foldl (\cs f -> f cs) cards0 $ map strToF $ lines instructionString
  -- print $ length cards
  -- print $ 2019 `elemIndex` cards

  putStrLn "Part 2"
  let deckSize = 119315717514047
      nShuffles = 101741582076661
      (a,b) = trace "many shuffles" $ traceShowId $
              shuffles deckSize nShuffles $
              trace "one shuffle" $ traceShowId $
              shuffle instructionString deckSize (1,0)
  print $ invert deckSize (a,b) 2020
