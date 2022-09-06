{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Game
  ( Game,
    newGame,
    isInitial,
    Game.isCheck,
    Game.isCheckmate,
    tryMovement,
    currentBoard,
    whoPlaysNow,
    update,
    turn,
    show,
    availableMoves,
    undo,
    player1,
    player2,
  )
where

import AI.BoardAI
import AI.GameAI
import Chess.Board
import qualified Chess.Board as Board
import Control.DeepSeq
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Player (Color (Black, White), Player, colorSwitch, getPlayerColor)

-----------------------------------------------------------------------------------------
-- GAME DEFINITIONS
-----------------------------------------------------------------------------------------

-- | Type definition for Game abstraction
data Game = Game
  { player1 :: Player,
    player2 :: Player,
    plays :: [Chess.Board.Board],
    turn :: Player.Color
  }
  deriving (Generic, NFData)

instance Show Game where
  show Game {player1 = _, player2 = _, plays = pp} = show $ head pp

instance AI.GameAI.ZeroSumGame Game where
  evaluateGame game = AI.BoardAI.evaluateBoard (currentBoard game) (turn game)
  nextGames game =
    if Game.isCheckmate game
      then []
      else
        let allPossibilities = Board.possibleMovementsForeachTeamPosition (currentBoard game) (turn game)
         in catMaybes $ concatMap (\(src, possibleDsts) -> map (tryMovement game src) possibleDsts) allPossibilities

-- | Initializes a blank Game
newGame :: Player.Player -> Player.Player -> Game
newGame p1 p2 = Game p1 p2 [] White

-- | Given a new board, progresses the game state by one move.
update :: Game -> Chess.Board.Board -> Game
update game nBoard =
  let p1 = player1 game
      p2 = player2 game
      newPlays = nBoard : plays game
      nextTurn = colorSwitch $ turn game
   in Game {player1 = p1, player2 = p2, plays = newPlays, turn = nextTurn}

-- | Undo the most recent move
undo :: Game -> Game
undo gg =
  let p1 = player1 gg
      p2 = player2 gg
      (_ : _ : previousPlays) = plays gg
   in Game {player1 = p1, player2 = p2, plays = previousPlays, turn = turn gg}

-- | Retrieves current game board from the Game structure
currentBoard :: Game -> Chess.Board.Board
currentBoard game = head $ plays game

-- | Get next move player
whoPlaysNow :: Game -> Player.Player
whoPlaysNow game =
  if turn game == White
    then playerByColor White game
    else playerByColor Black game
  where
    playerByColor cc gg = if getPlayerColor (player1 gg) == cc then player1 gg else player2 gg

-----------------------------------------------------------------------------------------
-- STATE CHECKERS
-----------------------------------------------------------------------------------------

-- | No moves have been made iff length of plays is 1
isInitial :: Game -> Bool
isInitial game = length (plays game) == 1

-- | Determine whether or not the current board for the provided game yields checkmate
isCheckmate :: Game -> Bool
isCheckmate game = Chess.Board.isCheckmate (currentBoard game) (turn game)

-- | Determines whether or not the current board for the provided game yields check
isCheck :: Game -> Bool
isCheck game = Board.isCheck (currentBoard game) (turn game)

-----------------------------------------------------------------------------------------
-- PROTECTED FUNCTIONALITY
-----------------------------------------------------------------------------------------

-- | Protected attempt on making a particular move give a board, starting position,
-- and desired position to move to
tryMovement :: Game -> Board.Position -> Board.Position -> Maybe Game
tryMovement game src post =
  case Board.movePiece (currentBoard game) (turn game) src post of
    Just nb -> Just (update game nb)
    Nothing -> Nothing

-- | Given a game, returns a list of possible moves for the current ```turn```
availableMoves :: Game -> Board.Position -> [Board.Position]
availableMoves game = Board.freeMovements (currentBoard game) (turn game)
