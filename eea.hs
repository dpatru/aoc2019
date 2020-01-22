import Debug.Trace

-- Extended euclidian algorithm

-- given integers a and b, find s and t such that sa + tb = gcd(a,b)

eea :: (Show a, Integral a) => a -> a -> (a, a, a, a)
eea a b | r == 0 = traceShow ("base", a, b) $ traceShowId $
          (0,1,b, b)
        | (s,t) == (0,1) = traceShow (a,b) $ traceShowId $
          (t, s-q, d, t*a + (s-q) *b)
        | otherwise = traceShow (a,b) $ traceShowId $
          (t, (s- q*t), d, t*a +(s-q*t) *b)
  where (q,r) = divMod a b
        (s,t,d,d') = eea b r


readInt :: String -> Int
readInt = read

main = do
  putStrLn "Enter two, space-separated numbers to find (s,t,gcd) with the extended euclidian algorithm"
  interact (unlines . map (show . (\[a,b]->eea a b) . map readInt . words) . lines)
