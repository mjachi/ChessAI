module Main (main) where

import Data.Char (digitToInt)
import Data.List (elemIndex, intercalate)
import Data.List.Split (splitOn)
import qualified Game
import qualified Player
import System.Console.CmdArgs
import qualified Utils

-- | Main
main :: IO ()
main = do
  arguments <- cmdArgs (modes [playerVPlayer, playerVComputer &= auto, computerVComputer]
    &= program "ChessAI" &= help "Chess game with weak engine")
  Utils.reset
  Utils.printWithColor "ChessAI -- welcome" "white"
  let (player1, player2) = playersFromArgs arguments
    game = Game.newGame player1 player2
  in performGame game

-- | Defines struct with which we can parse command line args
data GameArgs
  = PvP String String String
  | PvC String String Integer
  | CvC Integer Integer
  deriving (Show)

-- | Given a string, returns either corresponding player color. Must be one of "black",
-- "Black", "white", "White" or returns Nothing.
parseColor :: String -> Maybe Player.Color
parseColor s = case s of
  c
    | c `elem` ["black", "Black"] -> Just Player.Black
    | c `elem` ["white", "White"] -> Just Player.Black
    | otherwise -> Nothing

playerVPlayer = PvP "Player1" "white" "Player2"

playerVComputer = PvC "Player1" "white" 3

computerVComputer = CvC 3 3

-- | Construct Player 1 and 2 from the Args struct from abov.
playersFromArgs :: GameArgs -> (Player.Player, Player.Player)
playersFromArgs HvH {p1name = p1n, p1color = p1c, p2name = p2n} =
  (Player.mkHumanPlayer p1n (parseColor p1c), Player.mkHumanPlayer p2n (switch $ parseColor p1c))
playersFromArgs HvC {hname = hn, hcolor = hc, cstrength = cs} =
  (Player.mkHumanPlayer hn (parseColor hc), Player.mkComputerPlayer cs (switch $ parseColor hc))
playersFromArgs CvC {p1strength = p1s, p2strength = p2s} = 
  (Player.mkComputerPlayer p1s White, Player.mkComputerPlayer p2s Black)

-- | Main game loop
performGame :: Game.Game -> IO ()
performGame game = do
  printGameLayout game
  case Game.isCheckMate game of
    True  -> Screen.printWithColor ("Game finished! " ++ colorTurn game ++ " loses!") "red"
    False -> performGameTurn game $ Game.whoPlaysNow game

-- | IO loop to make a move
performGameTurn :: Game.Game -> Player.Player -> IO ()
performGameTurn game (Player.ComputerPlayer strength _) =
  let updatedGame = GameAI.performMovement game strength
   in performGame updatedGame
performGameTurn game (Player.HumanPlayer name _)    = do
  putStrLn "One of: exit, which, move, undo"
  userInput <- getLine
  case parseCommand userInput of
    ("exit", _) -> return ()
    ("which", pos:_) -> do
      Screen.printWithColor ("Available movements are: " ++ (showAvailableMovements game (parsePosition pos))) "white"
      Screen.pause
      performGame game
    ("move", src:dst:_) -> do
      performMoveAction game (parsePosition src) (parsePosition dst)
    ("undo", _) -> performUndoAction game
    _ -> do
      Screen.printError "Did not recognize input; please try again"
      performGame game

performUndoAction :: Game.Game -> IO ()
performUndoAction game =
  if Game.isInitial game
  then do
    Screen.printError "Can't undo game!"
    performGame game
  else do
    performGame $ Game.undo game

parseCommand :: String -> (String, [String])
parseCommand userInput = let command:args = splitOn " " userInput
                          in (command, args)

parsePosition :: String -> Maybe Position
parsePosition pos = if length pos == 2
                    then let a:b:_ = pos in parsePosition' a b
                    else Nothing
  where
    parsePosition' column row = do
      c <- (+1) <$> elemIndex column ['a'..'h']
      r <- (+9) <$> negate <$> safeDigitToInt row
      return (r, c)
    safeDigitToInt r = if elem r ['1'..'8']
                       then Just $ digitToInt r
                       else Nothing

-- | Wraps game functionality against IO to execute a move, error checking as needed
performMoveAction :: Game.Game -> Maybe Position -> Maybe Position -> IO ()
performMoveAction game Nothing dst = do
  Screen.printError "Bad source position"
  performGame game
performMoveAction game src Nothing = do
  Screen.printError "Bad destiny position"
  performGame game
performMoveAction game (Just src) (Just dst) =
  case (Game.tryMovement game src dst) of
    Just nextGame -> performGame nextGame
    Nothing -> do
      Screen.printError "Illegal movement. Try again."
      performGame game

showAvailableMovements :: Game.Game -> Maybe Position -> String
showAvailableMovements game Nothing         = "No movements"
showAvailableMovements game (Just position) =
  let movs = Game.availableMovements game position
   in if null movs then "No movements"
      else intercalate ", " $ map showPosition movs
  where showPosition (r, c) = (['a'..'h'] !! (c - 1)):(show (9 - r))

colorTurn :: Game.Game -> String
colorTurn game = case Game.turn game of
                   White -> "White"
                   Black -> "Black"

showCheckStatus :: Game.Game -> String
showCheckStatus game = if Game.isCheck game then " (and is in CHECK) "
                                            else ""
