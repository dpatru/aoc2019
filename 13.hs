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

run :: (Integer, Integer) -> Instructions -> [Integer] -> [Integer]
-- (instructionPointer, base), instructions, input, output
run (i, base) instructions inputs =
  -- traceShow (i, base, instructions) $
  case instr `mod` 100 of
    1 -> run (i+4, base) (insert (addr 3) (arg 1 + arg 2) instructions) inputs
    2 -> run (i+4, base) (insert (addr 3) (arg 1 * arg 2) instructions) inputs
    3 -> run (i+2, base) (insert (addr 1) (head inputs) instructions) $ tail inputs
    4 -> -- traceShow (arg 1) $
      arg 1: run (i+2, base) instructions inputs
    5 -> run (if arg 1 == 0 then i+3 else arg 2, base) instructions inputs
    6 -> run (if arg 1 == 0 then arg 2 else i+3, base) instructions inputs
    7 -> run (i+4, base) (insert (addr 3) (if arg 1 < arg 2 then 1 else 0) instructions) inputs
    8 -> run (i+4, base) (insert (addr 3) (if arg 1 == arg 2 then 1 else 0) instructions) inputs
    9 -> run (i+2, base+arg 1) instructions inputs
    99 -> []
    _ -> error "unknown opcode"
  where instr = instructions!i
        ii x = findWithDefault 0 x instructions
        arg :: Integer -> Integer
        arg n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ ii $ i+n
          1 -> ii $ i+n
          2 -> ii ((ii $ i+n) + base)
          _ -> error "bad argument mode"
        addr n = case (instr `mod` (100*10^n)) `div` (10*10^n) of
          0 -> ii $ i+n
          1 -> error "address in mode 1"
          2 -> --trace "address in mode 2" $
            ii (i+n) + base
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

type Game = Map (Integer, Integer) Integer
updateGame :: Game -> [Integer] -> Game
updateGame g [] = g
updateGame g (x:y:t:r) = updateGame (insert (x,y) t g) r
updateGame g _ = error "bad input"
                                    
display :: [Integer] -> IO ()
display drawCommands = runCurses $ do
    setEcho False
    w <- defaultWindow
    updateWindow w $ do
        draw drawCommands
    render
    waitFor w (\ev -> ev == EventCharacter 'q' || ev == EventCharacter 'Q')
      where draw [] = do
              moveCursor 0 10
              drawString "Done"
            draw (x:y:t:r) =
              if (x,y) == (-1, 0)
              then do 
                moveCursor 0 0
                drawString $ show t
                draw r
              else do
                moveCursor (y+1) x 
                drawString (case t of
                              0 -> " "
                              1 -> "W"
                              2 -> "B"
                              3 -> "_"
                              4 -> "*")
                draw r

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop where
    loop = do
        ev <- getEvent w Nothing
        case ev of
            Nothing -> loop
            Just ev' -> if p ev' then return () else loop

play :: [Integer] -> IO ([Integer])
play drawInstructions = runCurses $ do
  setEcho False
  w <- defaultWindow
  loop w drawInstructions
    where loop :: Window -> [Integer] -> Curses([Integer])
          loop w [] = do
            updateWindow w $ do
              moveCursor 0 10
              drawString "Press q to quit"
            render
            let qloop = do
                  ev <- getEvent w Nothing
                  case ev of
                    Nothing -> qloop
                    Just (EventCharacter 'q') -> return []
                    Just (EventCharacter 'Q') -> return []
                    _ -> qloop
            qloop
          loop w (x:y:t:r) = do
            updateWindow w $ do
              if (x,y) == (-1, 0)
                then do 
                moveCursor 0 0
                drawString $ show t
                else do
                moveCursor (y+1) x
                drawString (case t of
                              0 -> " "
                              1 -> "W"
                              2 -> "B"
                              3 -> "_"
                              4 -> "*"
                              _ -> error ("Unknown input " ++ show t)
                           )
            render
            moveEvent <- getEvent w (Just 1)
            let joystick = case moveEvent of
                             Just (EventSpecialKey KeyLeftArrow) -> -1
                             Just (EventSpecialKey KeyRightArrow) -> 1
                             Nothing -> 0
                             Just (EventCharacter 'j') -> -1
                             Just (EventCharacter 'J') -> -1
                             Just (EventCharacter 'k') -> 1
                             Just (EventCharacter 'K') -> 1
                             _ -> 0
            updateWindow w $ do
              moveCursor 0 10
              drawString $ case joystick of
                -1 -> "move left"
                0 -> "no move"
                1 -> "move right"
            render
            -- laterJoysticks <- loop r
            -- return (joystick: laterJoysticks)
            joystick `merge` loop w r
          loop w badQ = error $ "loop: bad q: " ++ show badQ
          merge:: Integer -> Curses([Integer]) -> Curses([Integer])
          merge j ~js' = do
            js <- js'
            return (j:js)


main = do
  [instructionFile] <- getArgs
  instructionStrings <- readFile instructionFile
  let instructions = fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings

  putStrLn "Part 1"
  let moves = run (0,0) instructions []
  putStrLn $ show moves
  let game = updateGame empty moves
  putStrLn $ show ("blocks", size (M.filter (== 2) game))
  
  -- putStrLn "Part 1b"
  -- display drawCommands
  -- putStrLn "Part 2"
  -- let instructions2 = insert 0 2 instructions -- play for free by setting memory address 0 to 2
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

