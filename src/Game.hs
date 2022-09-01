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
newGame p1 p2 = Game p1 p2 [] White

-- | Given a new board, progresses the game state by one move.
update :: Game -> Chess.Board.Board -> Game
update game nBoard =
  let p1 = player1 game
      p2 = player2 game
      newPlays = nBoard : plays game
      nextTurn = colorSwitch $ turn game
   in Game {player1 = p1, player2 = p2, plays = newPlays, turn = nextTurn}
