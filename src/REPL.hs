{-# LANGUAGE DeriveDataTypeable #-}

module REPL (mainWrap) where

import qualified AI.GameAI
import qualified Chess.Board
import Data.Char (digitToInt)
import Data.List (elemIndex, intercalate)
import Data.List.Split (splitOn)
import qualified Game
import Player (colorSwitch)
import qualified Player
import System.Console.CmdArgs
import qualified Utils

-- |
data Args
  = PvP {p1name :: String, p1color :: String, p2name :: String}
  | PvC {hname :: String, hcolor :: String, cstrength :: Integer}
  | CvC {p1strength :: Integer, p2strength :: Integer}
  deriving (Show, Data, Typeable)

-- | Player vs Player argument specification
playerVsPlayer :: Args
playerVsPlayer =
  PvP
    { p1name = "Player 1" &= help "The player 1's name",
      p1color = "white" &= help "The player 1's color",
      p2name = "Player 2" &= help "The player 2's name"
    }

-- | Player vs Computer argument specification
playerVsComputer :: Args
playerVsComputer =
  PvC
    { hname = "Player 1" &= help "The human player's name",
      hcolor = "white" &= help "The human player's color",
      cstrength = 3 &= help "The computer player's strength"
    }

-- | Computer vs Computer argument specification
computerVsComputer :: Args
computerVsComputer =
  CvC
    { p1strength = 3 &= help "The player 1's strength",
      p2strength = 3 &= help "The player 2's strength"
    }

-- | Given string representation of color, returns corresponding Player.Color value
parseColor :: String -> Player.Color
parseColor s = case s of
  c
    | c `elem` ["black", "Black"] -> Player.Black
    | c `elem` ["white", "White"] -> Player.Black

-- | Constructs Player instances from the args struct defined above
playersFromArgs :: Args -> (Player.Player, Player.Player)
playersFromArgs PvP {p1name = p1n, p1color = p1c, p2name = p2n} =
  let player1Name = Player.PlayerName {Player.name = p1n}
      player1Color = Player.PlayerColor {Player.c = parseColor p1c}
      player2Name = Player.PlayerName {Player.name = p2n}
      player2Color = Player.PlayerColor {Player.c = Player.colorSwitch (parseColor p1c)}
   in (Player.Human player1Name player1Color, Player.Human player2Name player2Color)
playersFromArgs PvC {hname = hn, hcolor = hc, cstrength = cs} =
  let player1Name = Player.PlayerName {Player.name = hn}
      player1Color = Player.PlayerColor {Player.c = parseColor hc}
      playerCStrength = Player.ComputerDifficulty {Player.val = cs}
      playerCColor = Player.PlayerColor {Player.c = colorSwitch (parseColor hc)}
   in (Player.Human player1Name player1Color, Player.Computer playerCStrength playerCColor)
playersFromArgs CvC {p1strength = p1s, p2strength = p2s} =
  let player1Strength = Player.ComputerDifficulty {Player.val = p1s}
      player2Strength = Player.ComputerDifficulty {Player.val = p2s}
      player1Color = Player.PlayerColor Player.White
      player2Color = Player.PlayerColor Player.Black
   in (Player.Computer player1Strength player1Color, Player.Computer player2Strength player2Color)

-----------------------------------------------------------------------------------------
-- PROGRAM
-----------------------------------------------------------------------------------------

-- | Wrapper from main to keep as much within src directory as possible
mainWrap :: IO ()
mainWrap = do
  arguments <- cmdArgs (modes [playerVsPlayer, playerVsComputer &= auto, computerVsComputer] &= program "ChessAI" &= help "Chess game with weak engine")
  Utils.reset
  Utils.cPrint "ChessAI ---" "white"
  let (player1, player2) = playersFromArgs arguments
      game = Game.newGame player1 player2
   in performGame game

-- | Main game "loop body"
performGame :: Game.Game -> IO ()
performGame game = do
  printGameLayout game
  ( if Game.isCheckmate game
      then Utils.cPrint ("Game finished! " ++ colorTurn game ++ " loses!") "red"
      else performGameTurn game $ Game.whoPlaysNow game
    )

-- | Prints out game loop
printGameLayout :: Game.Game -> IO ()
printGameLayout game = do
  Utils.setCursor 2 0
  Utils.clearUntilEnd
  putStrLn $ Game.show game
  Utils.cPrint (colorTurn game ++ " moves..." ++ showCheckStatus game) "white"

-- | Implementation dependent on game mode (eg between Computer and actual Player);
-- in both cases, serves to complete a game turn
performGameTurn :: Game.Game -> Player.Player -> IO ()
performGameTurn game (Player.Computer strength _) =
  let updatedGame = AI.GameAI.performMovement game (Player.val strength)
   in performGame updatedGame
performGameTurn game (Player.Human _ _) = do
  putStrLn "Commands are: exit, which, move, undo"
  userInput <- getLine
  case parseCommand userInput of
    ("exit", _) -> return ()
    ("which", pos : _) -> do
      Utils.cPrint ("Available movements are: " ++ showAvailableMovements game (parsePosition pos)) "white"
      Utils.pause 2000000
      performGame game
    ("move", src : dst : _) -> do
      performMoveAction game (parsePosition src) (parsePosition dst)
    ("undo", _) -> performUndoAction game
    _ -> do
      Utils.printError "Bad command. Try again."
      performGame game

-- | IO action to move game back one step
performUndoAction :: Game.Game -> IO ()
performUndoAction game =
  if Game.isInitial game
    then do
      Utils.printError "Can't undo game!"
      performGame game
    else do
      performGame $ Game.undo game

-- | String tokenizer
parseCommand :: String -> (String, [String])
parseCommand userInput =
  let command : aa = splitOn " " userInput
   in (command, aa)

-- | From string, produces
parsePosition :: String -> Maybe Chess.Board.Position
parsePosition pos =
  if length pos == 2
    then let a : b : _ = pos in parsePosition' a b
    else Nothing
  where
    parsePosition' column row = do
      c <- (+ 1) <$> elemIndex column ['a' .. 'h']
      r <- (+ 9) . negate <$> safeDigitToInt row
      return (r, c)
    safeDigitToInt r =
      if r `elem` ['1' .. '8']
        then Just $ digitToInt r
        else Nothing

-- |
performMoveAction :: Game.Game -> Maybe Chess.Board.Position -> Maybe Chess.Board.Position -> IO ()
performMoveAction game Nothing _ = do
  Utils.printError "Bad source position"
  performGame game
performMoveAction game _ Nothing = do
  Utils.printError "Bad destiny position"
  performGame game
performMoveAction game (Just src) (Just dst) =
  case Game.tryMovement game src dst of
    Just nextGame -> performGame nextGame
    Nothing -> do
      Utils.printError "Illegal movement. Try again."
      performGame game

showAvailableMovements :: Game.Game -> Maybe Chess.Board.Position -> String
showAvailableMovements _ Nothing = "No movements"
showAvailableMovements game (Just position) =
  let movs = Game.availableMoves game position
   in if null movs
        then "No movements"
        else intercalate ", " $ map showPosition movs
  where
    showPosition (r, c) = (['a' .. 'h'] !! (c - 1)) : show (9 - r)

colorTurn :: Game.Game -> String
colorTurn game = case Game.turn game of
  Player.White -> "White"
  Player.Black -> "Black"

showCheckStatus :: Game.Game -> String
showCheckStatus game =
  if Game.isCheck game
    then " (and is in CHECK) "
    else ""
