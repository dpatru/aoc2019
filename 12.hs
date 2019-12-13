
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
findRepeat :: Integer -> Pair -> Map Pair Integer -> (Integer, Integer)
findRepeat i p m
  | p `member` m = (m!p, i)
  | otherwise =  findRepeat (i+1) (step1 p) (insert p i m)

main = do
  ls <- getContents
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
  let (i1, j1) = findRepeat 0 (xs, vxs) empty
  putStrLn $ show (i1, j1)
  let x = j1 - i1 -- in case the cycle doesn't begin at 0, take the difference
  let (i2, j2) = findRepeat 0 (ys, vys) empty
  let y = j2 - i2
  putStrLn $ show (i2, j2)
  let gcd1 = gcd x y
  putStrLn $ show ("gcd1", gcd1)
  let (i3, j3) = findRepeat 0 (zs, vzs) empty
  let z = j3 - i3
  putStrLn $ show (i3, j3)
  let p1 = (x * y) `div` gcd1
  let gcd2 = gcd p1 z
  putStrLn $ show ("gcd2", gcd2)
  putStrLn $ show ("product", (p1 * z) `div` gcd2)
