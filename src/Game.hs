{-# LANGUAGE DeriveGeneric #-}



module Game where

import Chess.Board
import Control.DeepSeq
import GHC.Generics (Generic)

import Player




data Game = Game
  { player1 :: Player.Player,
    player2 :: Player.Player,
    plays :: [Chess.Board.Board],
    turn :: Player.Color
  } deriving (Generic)

