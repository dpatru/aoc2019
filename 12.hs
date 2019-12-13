
import Data.List (transpose)
import Data.Map.Strict (Map, empty, insert, (!), member)

type Pair = ([Int],[Int])
step1 :: Pair -> Pair
step1 (xs, vxs) = (xs', vxs')
  where diff x y = if x < y then 1 else if x == y then 0 else -1
        diffs x ys = sum [diff x y | y <- ys]
        vxs' = [vx + diffs x xs | (x, vx) <- zip xs vxs]
        xs' = [x + vx' | (x, vx') <- zip xs vxs']
        
step 0 pairs = pairs
step n pairs = step (n-1) $ map step1 pairs

energy [(xs, vxs), (ys, vys), (zs, vzs)] = sum [p * k | (p,k) <- zip potential kinetic]
  where potential = map (sum . map abs) $ transpose [xs, ys, zs]
        kinetic = map (sum . map abs) $ transpose [vxs, vys, vzs]

-- Use a Map to store previous states, because we aren't guaranteed
-- that the state will return to the initial state.
findRepeat :: Integer -> Map Pair Integer -> Pair -> Integer
findRepeat i m p  
  | p `member` m = i - m!p
  | otherwise =  findRepeat (i+1) (insert p i m) (step1 p)

-- Try to match the initial state only
findRepeat2 :: Pair -> Integer
findRepeat2 p0  = f 1 $ step1 p0
  where f i p = if p == p0 then i else f (i+1) (step1 p)

combineCycles cs = foldl (\x y -> x * y `div` gcd x y) 1 cs

main = do
  ls <- getContents -- input should be space-separated x y z coords,
                    -- one line per moon
  let [xs, ys, zs] = transpose $ map (map read . words) $ lines ls
  let [vxs, vys, vzs] = map (map (const 0)) [xs, ys, zs]
  let pairs = [(xs, vxs), (ys, vys), (zs, vzs)]
  putStrLn "Part 1"
  let pairs1000 = step 1000 pairs
  putStrLn $ show ("energy", energy pairs1000)

  putStrLn "Part 2"
  -- There are two tricks here. First, realize that each dimension is
  -- independent. The x coordinate depends only on other x
  -- coordinates, not y or z coordinates. This means that we can
  -- calculate the cycle for x, y, and z coordinates seperately, and
  -- then just multiply them to find the total cycle. Second, realize
  -- that the total cycle of two cycles is their product divided by
  -- the greatest common divisor. (To see why, consider c1 = i * g,
  -- and c2 = j * g, then c1 * c2 = i * j * g * g. Notice that g (gcd)
  -- appears twice in the product but is only needed once.)

  putStrLn $ show $ combineCycles $ map (findRepeat 0 empty) pairs
  putStrLn $ show $ combineCycles $ map findRepeat2 pairs
