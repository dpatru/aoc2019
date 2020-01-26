{-# LANGUAGE FlexibleInstances #-}

import Debug.Trace
-- import Control.Monad.State.Lazy

import System.Environment (getArgs)
import System.IO (readFile)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
--import qualified Data.Array as A
import Data.List (find, intercalate, intersperse, permutations, inits, tails, isPrefixOf, isInfixOf)
import Data.List.Split (splitOn)
-- import Data.Complex (Complex((:+)), realPart, imagPart) -- define my own complex

import Text.Regex.PCRE

import UI.NCurses
import Data.Char (chr, ord)
-- import Data.Complex (Complex((:+)))
import Data.Maybe (fromJust, isNothing, isJust)
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

toAscii ii = map (chr . fromIntegral) ii
fromAscii s = map (fromIntegral . ord) s

instance Show Computer where
  show c = "Computer < \""++toAscii (input c)++"\" > \""++toAscii (output c)++"\"\n"

computer0 = Computer { state = Ready, memory = M.empty, iptr = 0, base = 0, input = [], output = [] }

run :: Computer -> Computer -- (Integer, Integer) -> Instructions -> [Integer] -> [Integer]
run c
  | state c == Ready = -- traceShowId $
    run $ c {state = Running}
  | state c == Done = -- traceShowId $
    c
  | state c == Blocked = -- traceShowId $
    if input c == [] then c
    else run $ c {state = Running}
  | otherwise = -- running
    --traceShow (iptr c, base c, memory c) $ 
    case instr `mod` 100 of
      1 -> -- add
        run $ c {iptr = i+4, memory = M.insert (addr 3) (arg 1 + arg 2) instructions}
      2 -> -- multiply
        run $ c {iptr = i+4, memory = M.insert (addr 3) (arg 1 * arg 2) instructions}
      3 -> -- read input
        if (null $ input c)
        then c {state = Blocked}
        else run $ c {iptr = i+2, memory = M.insert (addr 1) (head $ input c) instructions, input = tail $ input c}
      4 -> -- output
        --traceShow (arg 1) $ 
        run $ c {iptr = i+2, output = output c ++ [arg 1]}
      5 -> -- jump-if-true
        run $ c {iptr = if arg 1 == 0 then i+3 else arg 2}
      6 -> -- jump-if-false
        run $ c {iptr = if arg 1 == 0 then arg 2 else i+3}
      7 -> -- less than
        run $ c {iptr = i+4, memory = M.insert (addr 3) (if arg 1 < arg 2 then 1 else 0) instructions}
      8 -> -- equals
        run $ c {iptr = i+4, memory = M.insert (addr 3) (if arg 1 == arg 2 then 1 else 0) instructions}
      9 -> -- set relative base
        run $ c {iptr = i+2, base = base c + arg 1}
      99 -> -- halt
        run $ c {state = Done}
      _ -> error "unknown opcode"
  where instructions = memory c
        i = iptr c
        instr = instructions!i
        ii x = M.findWithDefault 0 x instructions
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

type Direction = String

-- data Direction = N | S | E | W
-- instance Show Direction where
--   show N = "north"
--   show S = "south"
--   show E = "east"
--   show W = "west"
-- instance Read Direction where
--   readsPrec _ ('n':'o':'r':'t':'h':theRest) = [(N, theRest)]
--   readsPrec _ ('s':'o':'u':'t':'h':theRest) = [(S, theRest)]
--   readsPrec _ ('e':'a':'s':'t':theRest) = [(E, theRest)]
--   readsPrec _ ('w':'e':'s':'t':theRest) = [(W, theRest)]
  
type Graph = Map String (Map Direction (Maybe String))

back "north" = "south"
back "south" = "north"
back "east" = "west"
back "west" = "east"
back d = error $ "unknown door type " ++ d

stripRoomName = reverse . drop 3 . reverse . drop 3 -- strip first 3 and last 3 characters 
interactc :: Graph -> [(Direction, String)] -> Computer -> IO()
interactc g explorePath c = do
  let out = toAscii $ output c
      linesOut = lines out
      room = map stripRoomName $ filter (isPrefixOf "==") $ linesOut
      droidWeight
        | "Droids on this ship are heavier" `isInfixOf` out = "too light"
        | "Droids on this ship are lighter" `isInfixOf` out = "too heavy"
        | otherwise = "not applicable"
      -- if booted out, then last room listed is current room
      -- room = map (head . tail) $ (out =~ "== (.+?) ==" :: [[String]]) -- init $ tail $ head linesOut
      prompt = not $ null $ filter (== "Command?") linesOut
      stuff = map (drop 2) $ filter (isPrefixOf "- ") linesOut
      doors = filter (`elem` ["north","south","east","west"]) stuff
      items = filter (not . (`elem` ["infinite loop", "molten lava", "escape pod", "photons", "giant electromagnet"])) $ filter (not . (`elem` doors)) stuff
      newFrontier = not $ (null room || head room `M.member` g)
      (previousDoor, previousRoom) = head explorePath -- laziness ensures that explorePath is not null
      g' | newFrontier = M.insert (head room) (M.fromList [(door, Nothing)| door <- doors]) g
         | null room = g
         | null explorePath = trace "setting g': no explorePath" g
         | otherwise = M.adjust (\m -> M.insert (back $ previousDoor) (Just previousRoom) m) (head room) $
                       M.adjust (\m -> M.insert previousDoor (Just $ head room) m) previousRoom g
      (move, explorePath')
        | null room = ("to checkpoint", explorePath)
        | head room == "Security Checkpoint" = (back previousDoor, tail explorePath)
        | newFrontier = (head doors, (head doors, head room): explorePath)
        | null unexplored && null explorePath = ("inv", []) -- error "explored all"
        | null unexplored || head unexplored == previousRoom = (back previousDoor, tail explorePath)
        | otherwise = (head unexplored, (head unexplored, head room) : explorePath)
        where unexplored :: [Direction]
              unexplored = M.keys $ M.filter isNothing $ g!(head room)
  putStrLn out
  putStrLn $ "room: " ++ show room ++ "\nprompt: " ++ show prompt ++ "\ndoors: " ++ show doors ++ "\nitems: " ++ show items ++ "\nmove: " ++ show move ++ "\nexplorePath': " ++ show explorePath
  if not prompt then return ()
  else if move == "to checkpoint" then do
    putStrLn "Move to checkpoint"
    let pathToCheckpoint = traceShowId $ toCheckpoint g'
        c' = run $ c{output=[], input=concatMap ((\s -> s ++ [10]) . fromAscii) pathToCheckpoint}
    putStrLn $ toAscii $ output c'


    -- print $ g'
    -- s <- getLine
    -- interactc g' explorePath' $ run $ c{output=[], input=fromAscii s ++ [10]}
      
  else do
      if not (null items) then do
        putStrLn "Picking up items"
        let move' = "take " ++ head items
            c' = run $ c{output=[], input=fromAscii move' ++ [10]}
        putStrLn move'
        putStrLn $ toAscii $ output c'
        putStrLn move
        putStrLn $  "(explorePath = "++show explorePath++")"
        interactc g' explorePath' $ run $ c'{output=[], input=fromAscii move ++ [10]}
      else do
        putStrLn move
        putStrLn $  "(explorePath = "++show explorePath++")"
        interactc g' explorePath' $ run $ c{output=[], input=fromAscii move ++ [10]}

toCheckpoint g = search g "Security Checkpoint" $ [["Hull Breach"]]
  where search g goal paths
          | null paths = error "null paths"
          | room == goal = filter (`elem` ["north", "south", "east", "west"]) $ trace "path" $ traceShowId $ reverse path
          | null frontier = search g goal $ tail paths
          | otherwise = search g goal $ [room:door:path | (door, room) <- frontier] ++ tail paths
          where path = head paths
                room = head path
                frontier :: [(Direction, String)]
                frontier = M.toList $ M.filter (not . (`elem` path)) $ M.map fromJust $ M.filter isJust $ g!room
  
-- ("Arcade",fromList [("north",Just "Navigation"),("west",Just "Kitchen")]),
-- ("Corridor",fromList [("north",Just "Holodeck"),("west",Just "Gift Wrapping Center")]),
-- ("Crew Quarters",fromList [("north",Just "Engineering")]),
-- ("Engineering",fromList [("east",Just "Hull Breach"),("south",Just "Crew Quarters"),("west",Just "Hallway")]),
-- ("Gift Wrapping Center",fromList [("east",Just "Corridor"),("north",Just "Observatory"),("south",Just "Hull Breach")]),
-- ("Hallway",fromList [("east",Just "Engineering"),("north",Just "Science Lab"),("south",Just "Sick Bay")]),
-- ("Holodeck",fromList [("south",Just "Corridor")]),
-- ("Hot Chocolate Fountain",fromList [("east",Just "Storage")]),
-- ("Hull Breach",fromList [("east",Just "Kitchen"),("north",Just "Gift Wrapping Center"),("west",Just "Engineering")]),
-- ("Kitchen",fromList [("east",Just "Arcade"),("west",Just "Hull Breach")]),
-- ("Navigation",fromList [("south",Just "Arcade"),("west",Just "Storage")]),
-- ("Observatory",fromList [("north",Just "Passages"),("south",Just "Gift Wrapping Center")]),
-- ("Passages",fromList [("south",Just "Observatory")]),
-- ("Science Lab",fromList [("east",Just "Security Checkpoint"),("south",Just "Hallway")]),
-- ("Security Checkpoint",fromList [("east",Nothing),("west",Just "Science Lab")]),
-- ("Sick Bay",fromList [("east",Just "Warp Drive Maintenance"),("north",Just "Hallway")]),
-- ("Stables",fromList [("south",Just "Warp Drive Maintenance")]),
-- ("Storage",fromList [("east",Just "Navigation"),("west",Just "Hot Chocolate Fountain")]),
-- ("Warp Drive Maintenance",fromList [("north",Just "Stables"),("west",Just "Sick Bay")])]


main = do
  print $ toAscii $ fromAscii "This is a test"
  -- [instructionFile] <- getArgs
  instructionStrings <- readFile "25.input.txt" -- instructionFile
  let instructions = M.fromList . zip [0 ..] $ map read $ splitOn "," instructionStrings

  putStrLn "Part 1"
  interactc M.empty [] $ run $ computer0{memory = instructions}
  -- let c = run $ computer0{memory = instructions}
  -- let view = output c
  -- let viewStr = map (chr . fromIntegral) view
  -- putStrLn $ viewStr

