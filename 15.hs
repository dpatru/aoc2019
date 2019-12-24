import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Map.Strict (Map, (!), insert, elems, fromList, toList, findWithDefault, size, empty, member, findMin, findMax, singleton, filter)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
--import qualified Data.Array as A
import Data.List (permutations)
import Data.List.Split (splitOn)
import Data.Complex (Complex((:+)), realPart, imagPart)

import UI.NCurses


type Instructions = Map Integer Integer

data ComputerState = Ready | Running | Blocked | Done
  deriving (Show, Eq)

data Computer = Computer {
  state :: ComputerState,
  memory :: Map Integer Integer,
  iptr :: Integer, -- instruction pointer
  base :: Integer, -- base offset
  input :: [Integer],
  output :: [Integer]}

computer0 = Computer { state = Ready, memory = empty, iptr = 0, base = 0, input = [], output = [] }

run :: Computer -> Computer -- (Integer, Integer) -> Instructions -> [Integer] -> [Integer]
run c
  | state c == Ready = run $ c {state = Running}
  | state c == Done = c
  | state c == Blocked = if input c == [] then c else run $ c {state = Running}
  | otherwise = -- running
    --traceShow (iptr c, base c, memory c) $ 
    case instr `mod` 100 of
      1 -> -- add
        run $ c {iptr = i+4, memory = insert (addr 3) (arg 1 + arg 2) instructions}
      2 -> -- multiply
        run $ c {iptr = i+4, memory = insert (addr 3) (arg 1 * arg 2) instructions}
      3 -> -- read input
        if (null $ input c)
        then c {state = Blocked}
        else run $ c {iptr = i+2, memory = insert (addr 1) (head $ input c) instructions, input = tail $ input c}
      4 -> -- output
        --traceShow (arg 1) $ 
        run $ c {iptr = i+2, output = output c ++ [arg 1]}
      5 -> -- jump-if-true
        run $ c {iptr = if arg 1 == 0 then i+3 else arg 2}
      6 -> -- jump-if-false
        run $ c {iptr = if arg 1 == 0 then arg 2 else i+3}
      7 -> -- less than
        run $ c {iptr = i+4, memory = insert (addr 3) (if arg 1 < arg 2 then 1 else 0) instructions}
      8 -> -- equals
        run $ c {iptr = i+4, memory = insert (addr 3) (if arg 1 == arg 2 then 1 else 0) instructions}
      9 -> -- set relative base
        run $ c {iptr = i+2, base = base c + arg 1}
      99 -> -- halt
        run $ c {state = Done}
      _ -> error "unknown opcode"
  where instructions = memory c
        i = iptr c
        instr = instructions!i
        ii x = findWithDefault 0 x instructions
        arg :: Integer -> Integer
        arg n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ ii $ i+n
          1 -> ii $ i+n
          2 -> ii ((ii $ i+n) + base c)
          _ -> error "bad argument mode"
        addr n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ i+n
          1 -> error "address in mode 1"
          2 -> --trace "address in mode 2" $
            ii (i+n) + base c
          _ -> error $ "address in unknown mode"

instance Ord a => Ord (Complex a) where
  compare a b | ar < br || ar == br && ai < bi = LT
              | ar > br || ar == br && ai > bi = GT
              | otherwise = EQ
    where (ar, ai) = toParts a
          (br, bi) = toParts b

toParts :: Complex a -> (a, a)
toParts c = (realPart c, imagPart c)

toIntParts :: RealFrac a => Complex a -> (Int, Int)
toIntParts c = (round $ realPart c, round $ imagPart c)

fromParts :: (a, a) -> Complex a
fromParts (a,b) = a :+ b

data Game = Game
  { m :: Map (Integer, Integer) Integer -- map
  , x :: Integer
  , y :: Integer
  , minx :: Integer
  , miny :: Integer
  , maxx :: Integer
  , maxy :: Integer
  , explore :: [Int] -- stack of directions to explore
  } deriving (Show)

game0 = Game { m = empty, x = 0, y = 0, minx = 0, maxx = 0, miny = 0, maxy = 0, explore = [] }

updateGame :: Game -> Int -> Integer -> Game
updateGame g move status = updateMaxes $ case status of
  0 -> g { m = insert (x',y') 0 $ m g }
  1 -> g { m = insert (x',y') 1 $ m g, x = x', y = y'}
  2 -> g { m = insert (x',y') 2 $ m g, x = x', y = y'}
  where x' | move == 3 = x g - 1
           | move == 4 = x g + 1
           | otherwise = x g
        y' | move == 1 = y g + 1
           | move == 2 = y g - 1
           | otherwise = y g
        updateMaxes g = g
          { maxx = max x' $ maxx g
          , maxy = max y' $ maxy g
          , minx = min x' $ minx g    
          , miny = min y' $ miny g
          }

topMargin = 5

drawGame :: Window -> Game -> Curses()
drawGame w g = do
  updateWindow w $ do
    clear
    (width, height) <- windowSize
    let (width', height') = (maxx g - minx g, maxy g - miny g)
    resizeWindow (max width width') (max height height')
    sequence_ [ draw i j val
              | i <- [minx g .. maxx g]
              , j <- [miny g .. maxy g]
              , let val = findWithDefault 5 (i,j) $ m g
              ]
  render
    where draw i j val = do
            let (y',x') =(maxy g - j + topMargin, i - minx g)
            if x' < 0 then error $ "x < 0: " ++ show x'
              else if y' < 0 then error $ "y < 0: " ++ show y'
                   else moveCursor y' x'
            if (i,j) == (x g, y g)
              then drawString "D"
              else drawString $ case val of
                                  5 -> " "
                                  0 -> "#"
                                  1 -> "."
                                  2 -> "*"
                                  _ -> error $  "unknown map value: " ++ show val

suggestMove :: Game -> Int
suggestMove g = search candidates seen
  where seen :: Set (Integer, Integer)
        seen = S.fromList [(x g, y g)]
        candidates :: Set (Int, Int, Integer, Integer) -- steps, direction, x, y
        candidates = S.fromList [ (1, 1, x g, y g + 1)
                                , (1, 2, x g, y g - 1)
                                , (1, 3, x g - 1, y g)
                                , (1, 4, x g + 1, y g)
                                ]
        search :: Set (Int, Int, Integer, Integer) -> Set (Integer, Integer) -> Int
        search candidates seen
          | S.null candidates = 5 -- error "explored all"
          | otherwise = let ((steps, direction, x', y'), candidates') = S.deleteFindMin candidates in
              if (x', y') `S.member` seen then search candidates' seen
              else
                let seen' :: Set (Integer, Integer)
                    seen' = S.insert (x',y') seen
                    candidates'' =
                      foldr S.insert candidates' [(steps+1, direction, x'+i, y'+j)| (i,j) <- [(-1,0),(0,-1),(1,0),(0,1)]]
                in case M.lookup (x',y') (m g) of
                     Nothing -> direction -- not explored at all
                     Just 0 -> search candidates' seen' -- wall
                     _ -> search candidates'' seen'

play :: Game -> Computer -> IO (Game)
play game c = runCurses $ do
  setEcho False
  w <- defaultWindow
  loop w game $ run c
    where loop :: Window -> Game -> Computer -> Curses(Game)
          loop w g c
            | state c == Done = do
                drawGame w g
                updateWindow w $ do
                  moveCursor 0 10
                  drawString "Press q to quit"
                render
                let qloop = do
                      ev <- getEvent w Nothing
                      case ev of
                        Nothing -> qloop
                        Just (EventCharacter 'q') -> return g
                        Just (EventCharacter 'Q') -> return g
                        _ -> qloop
                qloop
            | state c == Blocked = do -- waiting for input
                drawGame w g
                updateWindow w $ do
                  moveCursor 0 10
                  drawString "Move"
                render
                let moveLoop delay = do
                      moveEvent <- getEvent w  $ delay
                      case moveEvent of
                        Just (EventSpecialKey KeyUpArrow) -> return 1
                        Just (EventSpecialKey KeyDownArrow) -> return 2
                        Just (EventSpecialKey KeyLeftArrow) -> return 3
                        Just (EventSpecialKey KeyRightArrow) -> return 4
                        Nothing -> return $ suggestMove g
                        _ -> moveLoop delay
                move <- moveLoop $ Just 0
                if move == 5
                  then loop w g $ c{state = Done}
                  else let c' = run $ c {input = [fromIntegral move], output = []} in 
                         loop w (updateGame g move $ head $ output c') $ c'{ input = [], output = [] }

            | otherwise = loop w game $ run c

type Point = (Integer,Integer)
type SearchState = (Int, Point)

main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings
  putStrLn "Part 1"
  game <- play game0 (computer0{memory = instructions})
  let search :: Set SearchState -> Set Point ->  SearchState
      search stateHeap seen
        | (m game)!(i,j) == 2 = (steps, (i,j)) -- found the goal, return the number of steps needed to reach it.
        | (m game)!(i,j) == 0 = search stateHeap' seen' -- testing a wall, don't add to the frontier
        | otherwise = search stateHeap'' seen' -- testing a clear spot, expand the frontier and keep searching
        where ((steps, (i,j)), stateHeap') = S.deleteFindMin stateHeap
              stateHeap'' = foldr S.insert stateHeap' $ frontier
              frontier = [(steps+1, pt)
                         | pt <- [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
                         , not $ pt `S.member` seen']
              seen' = S.insert (i,j) seen
  let (steps, oxygenOrigin) = search (S.singleton (0, (0,0))) S.empty
  putStrLn $ show $ (steps, oxygenOrigin)
  putStrLn "Part 2"
  let spread :: Set SearchState -> Set Point -> SearchState
      spread stateHeap seen
        | null stateHeap = (0, (0,0)) -- base case
        | otherwise = max (steps, (i,j)) $ spread stateHeap'' seen' -- testing a clear spot, expand the frontier and keep searching
        where ((steps, (i,j)), stateHeap') = -- traceShow stateHeap $
                S.deleteFindMin stateHeap
              stateHeap'' = foldr S.insert stateHeap' $ frontier
              frontier = [(steps+1, pt)
                         | pt <- [(i+1,j), (i-1,j), (i,j+1), (i,j-1)]
                         , not $ pt `S.member` seen'
                         , not $ (m game)!pt == 0 ]
              seen' = S.insert (i,j) seen
  let (steps, lastSpot) = spread (S.singleton (0, oxygenOrigin)) S.empty
  putStrLn $ show $ (steps, lastSpot)
      
  
