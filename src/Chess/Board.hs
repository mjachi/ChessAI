{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess.Board where

import Control.DeepSeq
import qualified Data.List as List (intersect)
import Data.Matrix
import GHC.Generics
import Player (Color (Black, White))

-----------------------------------------------------------------------------------------
-- BOARD TYPE DEFS
-----------------------------------------------------------------------------------------

-- | Chess peices include Pawn, Knight, Bishop, Rook, Queen, King.
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

-- | Colored Piece. Associates a piece to a color, meaning a player assumes ownership/ control thereof.
newtype CPiece = CPiece (Color, Piece) deriving (Eq, Generic)

instance Show CPiece where
  show (CPiece (color, piece)) = show color ++ show piece

-- | A position is by the piece's coordinates
type Position = (Int, Int)

-- | Square in the Board Matrix is either occupied or black
data Square = BlankSquare | OccupiedSquare CPiece

instance Show Square where
  show BlankSquare = "_"
  show (OccupiedSquare cp) = show cp

-- | A chessboard is to be defined as an 8x8 matrix
type Board = Matrix Square

-- | Construct a new, blank chessboard
newBoard :: (Position -> Square) -> Board
newBoard = matrix 8 8

-- | Constructs a board from provided arguments.
newBoardFromList :: [Square] -> Board
newBoardFromList = fromList 8 8

---------------------------------------------------------------------------------------
-- BOARD INITS
---------------------------------------------------------------------------------------

classicBoard :: Board
classicBoard = newBoard classicLayout

-- | Defines the classic chess layout, coloring pieces, etc.
classicLayout :: Position -> Square
classicLayout (i, j)
  | (i == 1) && (j == 1 || j == 8) = OccupiedSquare $ CPiece (Black, Rook)
  | (i == 1) && (j == 2 || j == 7) = OccupiedSquare $ CPiece (Black, Knight)
  | (i == 1) && (j == 3 || j == 6) = OccupiedSquare $ CPiece (Black, Bishop)
  | (i == 1) && (j == 4) = OccupiedSquare $ CPiece (Black, Queen)
  | (i == 1) && (j == 5) = OccupiedSquare $ CPiece (Black, King)
  | i == 2 = OccupiedSquare $ CPiece (Black, Pawn)
  | (i == 8) && (j == 1 || j == 8) = OccupiedSquare $ CPiece (White, Rook)
  | (i == 8) && (j == 2 || j == 7) = OccupiedSquare $ CPiece (White, Knight)
  | (i == 8) && (j == 3 || j == 6) = OccupiedSquare $ CPiece (White, Bishop)
  | (i == 8) && (j == 4) = OccupiedSquare $ CPiece (White, Queen)
  | (i == 8) && (j == 5) = OccupiedSquare $ CPiece (White, King)
  | i == 7 = OccupiedSquare $ CPiece (White, Pawn)
  | otherwise = BlankSquare

-----------------------------------------------------------------------------------------
-- BOARD FUNCTIONALITY
-----------------------------------------------------------------------------------------
