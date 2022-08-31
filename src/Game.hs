{-# LANGUAGE DeriveGeneric #-}

module Game where

import Chess.Board
import GHC.Generics (Generic)
import Player (Color (Black, White), Player, colorSwitch)

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
  deriving (Generic)

-- | Initializes a blank Game
newGame :: Player.Player -> Player.Player -> Game
newGame p1 p2 = new' p1 p2 [] White

-- | Given a new board, progresses the game state by one move.
updateGame :: Game -> Chess.Board.Board -> Game
updateGame game newBoard =
  let p1 = player1 game
      p2 = player2 game
      newPlays = newBoard : plays game
      nextTurn = colorSwitch $ turn game
   in newGame' p1 p2 newPlays nextTurn
