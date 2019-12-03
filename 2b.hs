#!/usr/bin/env stack
-- stack --resolver lts-14.16 script

import Matrix.Vector (fromList)
import Data.Array (Array, (!), (//), elems)
--import qualified Data.Array as A
import Data.List.Split (splitOn)

run :: Int -> Array Int Int -> Int
run i a = case a!i of
  1 -> run (i+4) $ a//[(a!(i+3), a!(a!(i+1)) + a!(a!(i+2)))]
  2 -> run (i+4) $ a//[(a!(i+3), a!(a!(i+1)) * a!(a!(i+2)))]
  99 -> a!0
  _ -> error "unknown opcode"


main = interact $ newln . show . test a . fromList . map read . splitOn ","
  where newln s = s ++ "\n"
        a = 19690720
        test answer code =
          head [(noun, verb, 100 * noun + verb)
               | noun <- [0 .. 99]
               , verb <- [0 .. 99]
               , run 0 (code//[(1,noun),(2,verb)]) == answer]
                             
                          
