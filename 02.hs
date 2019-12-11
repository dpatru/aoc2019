#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import Matrix.Vector (fromList)
import Data.Array (Array, (!), (//), elems)
--import qualified Data.Array as A
import Data.List.Split (splitOn)

run :: Int -> Array Int Int -> Array Int Int
run i a = case a!i of
  1 -> run (i+4) $ a//[(a!(i+3), a!(a!(i+1)) + a!(a!(i+2)))]
  2 -> run (i+4) $ a//[(a!(i+3), a!(a!(i+1)) * a!(a!(i+2)))]
  99 -> a
  _ -> error "unknown opcode"

--main = interact $ newln . show . (! 0) . run 0 . fromList . map read . splitOn ","
main = interact $ newln . show . elems . run 0 . fromList . map read . splitOn ","
  where newln s = s ++ "\n"
