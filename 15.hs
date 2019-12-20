import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Map.Strict (Map, (!), insert, elems, fromList, toList, findWithDefault, size, empty, member, findMin, findMax, singleton, filter)
import qualified Data.Map.Strict as M
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
  } deriving (Show)

game0 = Game { m = empty, x = 0, y = 0 }

updateGame :: Game -> Integer -> Integer -> Game
updateGame g move status = updateMaxes $ case status of
  0 -> g { m = insert (x',y') 0 $ g m }
  1 -> g { m = insert (x',y') 1 $ g m, x = x', y = y'}
  2 -> g { m = insert (x',y') 2 $ g m, x = x', y = y'}
  where x' | move == 3 = g x - 1
           | move == 4 = g x + 1
           | otherwise = g x
        y' | move == 1 = g y + 1
           | move == 2 = g y - 1
           | otherwise = g y
        updateMaxes g = g
          { maxx = max x' $ maxx g
          , maxy = max y' $ maxy g
          , minx = min x' $ minx g    
          , miny = min y' $ miny g
          }

topMargin = 5

drawGame Window -> Game => Curses()
drawGame w g = updateWindow w $ do
  sequence_ [ draw i j val
            | i <- [minx g .. maxx g]
            , j <- [miny g .. maxy g]
            , let val = findWithDefault -1 (i,j) $ m g
            ]
  render
    where draw i j val = do
            moveCursor (j + topMargin) i
            if (i,j) == (x g, y g)
              then drawString "D"
              else drawString $ case val of
                                  -1 -> " "
                                  0 -> "#"
                                  1 -> "."
                                  2 -> "*"
                                  _ -> error $  "unknown map value: " ++ show val

play :: Game -> Computer -> IO (Game)
play game c = runCurses $ do
  setEcho False
  w <- defaultWindow
  loop w game $ run c
    where loop :: Window -> Game -> Computer -> Curses(Game)
          loop w g c
            | state c == Done = do
                processDrawCommands w g' $ output c
                updateWindow w $ do
                  moveCursor 0 10
                  drawString "Press q to quit"
                render
                let qloop = do
                      ev <- getEvent w Nothing
                      case ev of
                        Nothing -> qloop
                        Just (EventCharacter 'q') -> return g'
                        Just (EventCharacter 'Q') -> return g'
                        _ -> qloop
                qloop
            | state c == Blocked = do -- waiting for input
                drawGame w g
                updateWindow w $ do
                  moveCursor 0 10
                  drawString "Move"
                render
                let moveLoop = do
                      moveEvent <- getEvent w Nothing
                      case moveEvent of
                        Just (EventSpecialKey KeyUpArrow) -> return 1
                        Just (EventSpecialKey KeyDownArrow) -> return 2
                        Just (EventSpecialKey KeyLeftArrow) -> return 3
                        Just (EventSpecialKey KeyRightArrow) -> return 4
                        _ -> moveLoop
                move <- moveLoop
                c' <- run $ c {input = [move]}
                loop w (updateGame g (output c') $ c' {output=[]}
            | otherwise = loop w game $ run c


main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings

  putStrLn "Part 1"
  let c = run $ computer0 {memory = instructions}
  -- putStrLn $ show ("state", state c, "drawCommands", output c)
  let game = updateGame game0 $ output c
  putStrLn $ show $ size $ M.filter (== 2) $ m game
  putStrLn "Part 1b"
  display $ output c
  putStrLn "Part 2"
  let instructions2 = insert 0 2 instructions -- play for free by setting memory address 0 to 2
  game <- play game0 $ computer0 {memory = instructions2}
  putStrLn $ show $ head $ p game
  -- let moves = moves >>= (play . run (0,0) instructions2)
  -- -- moves <- play $ run (0,0) instructions2 $ repeat 0
  -- moves' <- moves
  -- putStrLn $ show $ length moves'
  
  -- let moves = moves >>= (play . run (0,0) instructions2)
  -- moves' <- moves
  -- putStrLn $ show moves'
  -- moves' <- play $ run (0,0) instructions2 []
  -- putStrLn $ show ("moves", moves')
  -- let moves = run (0,0) instructions2 $ repeat 0
  -- putStrLn $ show ("moves", moves)
  -- display moves
  
  
  -- let out = run (0,0) instructions $ (startColors!origin) : processOutput
  --     (processOutput, finalp, finald, colors) = process (origin,up, startColors) $ out
  -- putStrLn
  --   $ unlines
  --   $ ["done Part 1"
  --     , show $ ("tiles painted: ", size colors)
  --     , show $ ("finalp", finalp, "finald", robot finald)
  --     ]
  -- putStr $ showColors finalp finald colors

  -- putStrLn "Part 2"
  -- let out = run (0,0) instructions $ (startColors2!origin) : processOutput
  --     (processOutput, finalp, finald, colors) = process (origin,up, startColors2) $ out
  -- putStrLn
  --   $ unlines
  --   $ ["done Part 2"
  --     , show $ ("tiles painted: ", size colors)
  --     , show $ ("finalp", finalp, "finald", robot finald)
  --     ]
  -- putStr $ showColors finalp finald colors

