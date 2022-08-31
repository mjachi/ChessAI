{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess.Board where

import Control.DeepSeq
import qualified Data.List as List (intersect)
import Data.Matrix
import GHC.Generics

type Board = Matrix Square

type Position = (Int, Int)

data Square = BlankSquare | OccupiedSquare

data Piece = Pawn | Knight | Bishop | Rook | Queen | King
  deriving (Eq, Generic, NFData)

instance Show Piece where
  show x = case x of
    Pawn -> "p"
    Knight -> "N"
    Bishop -> "B"
    Rook -> "R"
    Queen -> "Q"
    King -> "K"
