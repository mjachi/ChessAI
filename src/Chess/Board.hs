{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess.Board where

-----------------------------------------------------------------------------------------
-- Module defines the Chessboard, a few relevant types, and basic functionalities. See
-- the module Move for implementations of checking logic.
-----------------------------------------------------------------------------------------

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

-- | Colored Piece. Associates a piece to a color, meaning a player assumes ownership/
-- control thereof.
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

-- | Given a board, starting position, and resulting position, moves piece at A
--   to B by blanking out A. Does not check validity.
placePiece :: Board -> Position -> Position -> Board
placePiece board positionFrom positionTo =
  let srcSquare = board ! positionFrom
   in setElem BlankSquare positionFrom (setElem srcSquare positionTo board)

-----------------------------------------------------------------------------------------
-- LEGAL MOVES BY PIECE
-----------------------------------------------------------------------------------------

-- | Returns legal moves for the black side pawns; must be pruned given greater context
bPawnMovements :: Position -> [[Position]]
bPawnMovements (i, j) =
  if i == 2
    then [[(i + 1, j), (i + 2, j)]]
    else [[(i + 1, j)]]

-- | Returns legal moves for the white side pawns; must be pruned given greater context
wPawnMovements :: Position -> [[Position]]
wPawnMovements (i, j) =
  if i == 7
    then [[(i - 1, j), (i - 2, j)]]
    else [[(i - 1, j)]]

-- | Returns legal moves for a Knight piece; must be pruned given greater context
knightMovements :: Position -> [[Position]]
knightMovements (i, j) =
  let factors = [(x, y) | x <- [-2, -1, 1, 2], y <- [-2, -1, 1, 2], abs x + abs y == 3]
   in [[(i + x, j + y)] | (x, y) <- factors]

-- | Returns legal moves for a Bishop piece; must be pruned given greater context
bishopMovements :: Position -> [[Position]]
bishopMovements (i, j) =
  let upperLeft = zip (reverse [1 .. i - 1]) (reverse [1 .. j - 1])
      upperRight = zip (reverse [1 .. i - 1]) [j + 1 .. 8]
      lowerLeft = zip [i + 1 .. 8] (reverse [1 .. j - 1])
      lowerRight = zip [i + 1 .. 8] [j + 1 .. 8]
   in [upperLeft, upperRight, lowerLeft, lowerRight]

-- | Returns legal moves for a Rook piece; must be pruned given greater context
rookMovements :: Position -> [[Position]]
rookMovements (i, j) =
  let up = [(x, y) | x <- reverse [1 .. i - 1], y <- [j]]
      down = [(x, y) | x <- [i + 1 .. 8], y <- [j]]
      left = [(x, y) | x <- [i], y <- reverse [1 .. j - 1]]
      right = [(x, y) | x <- [i], y <- [j + 1 .. 8]]
   in [up, down, left, right]

-- | Returns legal moves for a Queen piece; must be pruned given greater context
queenMovements :: Position -> [[Position]]
queenMovements (i, j) = bishopMovements (i, j) ++ rookMovements (i, j)

-- | Returns legal moves for a King piece; must be pruned given greater context
kingMovements :: Position -> [[Position]]
kingMovements (i, j) =
  let firstSteps positions = ([head positions | not (null positions)])
   in map firstSteps $ queenMovements (i, j)
