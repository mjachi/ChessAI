{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Chess.Board
  ( Board,
    Position,
    Square (BlankSquare, OccupiedSquare),
    Piece (Pawn, Rook, Knight, Bishop, Queen, King),
    CPiece (CPiece),
    newBoard,
    newBoardFromList,
    classicBoard,
    legalGrab,
    freeMovements,
    possibleMovementsForeachTeamPosition,
    ccMovements,
    enemyMovements,
    allMovements,
    movePiece,
    isCheck,
    isCheckmate,
    placePiece,
    isValid,
    piece,
    square,
    isSquareFree,
    isSquareOccupiedByEnemy,
    positionsOf,
  )
where

-----------------------------------------------------------------------------------------
-- Module defines the Chessboard, a few relevant types, and basic functionalities. See
-- the module Move for implementations of checking logic.
-----------------------------------------------------------------------------------------

import Control.DeepSeq
import Data.Matrix
import GHC.Generics
import Player (Color (Black, White), colorSwitch)

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
newtype CPiece = CPiece (Color, Piece) deriving (Eq, Generic, NFData)

instance Show CPiece where
  show (CPiece (cc, pp)) = show cc ++ show pp

-- | A position is by the piece's coordinates
type Position = (Int, Int)

-- | Square in the Board Matrix is either occupied or black
data Square = BlankSquare | OccupiedSquare CPiece
  deriving (Eq, Generic, NFData)

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

legalGrab :: Board -> Color -> Position -> Bool
legalGrab board turn pos =
  let sq = uncurry safeGet pos board
   in case sq of
        Just BlankSquare -> False
        Just jsq -> not $ isSquareOccupiedByEnemy jsq turn
        Nothing -> False

movePiece :: Board -> Color -> Position -> Position -> Maybe Board
movePiece board turn src dst =
  let availableMovements = freeMovements board turn src
   in if dst `elem` availableMovements
        then Just $ treatPromotions $ placePiece board src dst
        else Nothing

-- TODO -- generic piece promotion, as opposed to necessitating a Queen
treatPromotions :: Board -> Board
treatPromotions board = mapRow (promotePawns White) 1 $ mapRow (promotePawns Black) 8 board
  where
    promotePawns cc _ sq =
      if isSquareOccupiedByPiece cc Pawn sq
        then OccupiedSquare (CPiece (cc, Queen))
        else sq

-- | Given a board, starting position, and resulting position, moves piece at A
--   to B by blanking out A. Does not check validity.
placePiece :: Board -> Position -> Position -> Board
placePiece board positionFrom positionTo =
  let srcSquare = board ! positionFrom
   in setElem BlankSquare positionFrom (setElem srcSquare positionTo board)

-- | Returns a list of Positions denoting the grid forming the chessboard as proxy
-- to the valid Position's on the board
validChessPositions :: [Position]
validChessPositions = [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]

-- | Returns a list of postitions for a particular colored piece
positionsOf :: Board -> Color -> Piece -> [Position]
positionsOf board cc somePiece =
  let occupiedPositions = filter (not . isSquareFree board) validChessPositions
      predicate position = CPiece (cc, somePiece) == piece (square board position)
   in filter predicate occupiedPositions

-- | Given a board and a color, directly returns list of enemy's legal moves
enemyMovements :: Board -> Color -> [Position]
enemyMovements board cc =
  let enemyColor = colorSwitch cc
   in concatMap (uncheckedFreeMovements board enemyColor) (ccPositions board enemyColor)

-----------------------------------------------------------------------------------------
-- GETTERS
-----------------------------------------------------------------------------------------

-- | Given a board and position, returns the square on that board at that position
square :: Board -> Position -> Square
square board pos = board ! pos

-- | Given a square, returns the piece on it
piece :: Square -> CPiece
piece (OccupiedSquare colorPiece) = colorPiece

-- | Given a square, returns its color
color :: Square -> Maybe Color
color (OccupiedSquare (CPiece (c, _))) = Just c
color BlankSquare = Nothing

-----------------------------------------------------------------------------------------
-- STATE CHECKERS
-----------------------------------------------------------------------------------------

-- | Determine if the current board is in a valid state
isValid :: Board -> Bool
isValid board =
  let blackKingPos = head $ positionsOf board Black King
      whiteKingPos = head $ positionsOf board White King
   in not (isPositionThreatened board White blackKingPos || isPositionThreatened board Black whiteKingPos)

-- | Determines whether or not the current position has ```Color``` ```cc``` in check
isCheck :: Board -> Color -> Bool
isCheck board cc = isPositionThreatened board cc $ head $ positionsOf board cc King

-- | Determines whether or not the given Board denotes a completed game ie in checkmate
isCheckmate :: Board -> Color -> Bool
isCheckmate board cc = isCheck board cc && null (ccMovements board cc)

-- | Determines whether or not the current position has a ```Position``` ```position```
-- threatened for ```Color``` ```cc```
isPositionThreatened :: Board -> Color -> Position -> Bool
isPositionThreatened board cc position = position `elem` enemyMovements board cc

-- | Given a Board and a Position, determines whether or not the corresponding Square is
-- empty or not (whether or not it is a BlankSquare)
isSquareFree :: Board -> Position -> Bool
isSquareFree board pos = square board pos == BlankSquare

-- | Given a Square and a Color, determines if the piece on that Square is of the
-- Color argument ```c```
isSquareOccupiedByColor :: Color -> Square -> Bool
isSquareOccupiedByColor _ BlankSquare = False
-- TODO: below definition needs Maybe checking
isSquareOccupiedByColor c sq = isColor c $ piece sq

-- | Given a Color, Piece, and a Square, determines if the Square is occupied by that
-- type of Piece of that Color
isSquareOccupiedByPiece :: Color -> Piece -> Square -> Bool
isSquareOccupiedByPiece _ _ BlankSquare = False
isSquareOccupiedByPiece c p (OccupiedSquare (CPiece (cc, pp))) = cc == c && pp == p

-- | Given a Square and a Color, determines if the Square is occupied by a Piece of
-- opposite Color; if BlankSquare, then false.
isSquareOccupiedByEnemy :: Square -> Color -> Bool
isSquareOccupiedByEnemy (OccupiedSquare (CPiece (Black, _))) White = True
isSquareOccupiedByEnemy (OccupiedSquare (CPiece (White, _))) Black = True
isSquareOccupiedByEnemy _ _ = False

-- | Given a Color and a CPiece, determines if the given CPiece is of that Color
isColor :: Color -> CPiece -> Bool
isColor c (CPiece (pc, _)) = pc == c

-- | Determines if the given Position pos on the Board board is occupied by a King
isKingOccupied :: Board -> Position -> Bool
isKingOccupied board pos =
  isOccupiedBy Black King board pos || isOccupiedBy White King board pos

-- | Determines if the given Position pos on the Board board is occupied by a Knight
isKnightOccupied :: Board -> Position -> Bool
isKnightOccupied board pos =
  isOccupiedBy Black Knight board pos || isOccupiedBy White Knight board pos

-- | Determines if the given Position pos on the Board board is occupied by a Pawn
isPawnOccupied :: Board -> Position -> Bool
isPawnOccupied board pos =
  isOccupiedBy Black Pawn board pos || isOccupiedBy White Pawn board pos

-- | Given a Color cc, Piece pp, Board board, and position pp, determines if pp is
-- occupied by that CPiece (colored piece)
isOccupiedBy :: Color -> Piece -> Board -> Position -> Bool
isOccupiedBy cc pp board pos =
  let coloredPiece = square board pos
   in coloredPiece == OccupiedSquare (CPiece (cc, pp))

-- | Determiens if a given Position is in the board (of course, a component of validity)
isInsideBoard :: Position -> Bool
isInsideBoard (i, j) = i >= 1 && i <= 8 && j >= 1 && j <= 8

-- | Determines if a given position on a Board is occupied by an enemy piece
isPositionOccupiedByEnemy :: Board -> Color -> Position -> Bool
isPositionOccupiedByEnemy board cc position = isSquareOccupiedByEnemy (square board position) cc

-----------------------------------------------------------------------------------------
-- MOVE FUNCTIONS
-----------------------------------------------------------------------------------------

allMovements :: CPiece -> Position -> [[Position]]
allMovements cp pos = map (filter isInsideBoard) $ pieceUnboundedMovements cp pos

availabilityPositionFilter :: Board -> Color -> [Position] -> [Position]
availabilityPositionFilter board turn positions =
  let (frees, occupied) = span (isSquareFree board) positions
   in if (not . null) occupied && isPositionOccupiedByEnemy board turn (head occupied)
        then head occupied : frees
        else frees

-- | List of a
ccPositions :: Board -> Color -> [Position]
ccPositions board cc = [position | position <- validChessPositions, isSquareOccupiedByColor cc $ square board position]

possibleMovementsForeachTeamPosition :: Board -> Color -> [(Position, [Position])]
possibleMovementsForeachTeamPosition board cc = map (\src -> (src, freeMovements board cc src)) (ccPositions board cc)

-- | Return list of movements for a given Color given a Board
ccMovements :: Board -> Color -> [Position]
ccMovements board cc = concatMap (freeMovements board cc) (ccPositions board cc)

-- | Returns a list of the possible free movements; takes advantage of helper
-- ```uncheckedFreeMovements``` to this end.
freeMovements :: Board -> Color -> Position -> [Position]
freeMovements board turn position =
  filter (\possibleDst -> not $ isCheck (placePiece board position possibleDst) turn) $
    uncheckedFreeMovements
      board
      turn
      position

-- | Returns list of possible movements without considerations on King being in check
uncheckedFreeMovements :: Board -> Color -> Position -> [Position]
uncheckedFreeMovements board cc pos
  | not (legalGrab board cc pos) = []
  | isKnightOccupied board pos = _knightFreeMovements board cc pos
  | isPawnOccupied board pos = _pawnFreeMovements board cc pos
  | otherwise = _generalFreeMovements board cc pos

-- | Retrieve list of free movements for a knight
_knightFreeMovements :: Board -> Color -> Position -> [Position]
_knightFreeMovements board turn position =
  let isOccupiedByMe = isSquareOccupiedByColor turn . square board
   in concatMap (filter (not . isOccupiedByMe)) (allMovements colouredPiece position)
  where
    selectedSquare = square board position
    colouredPiece = piece selectedSquare

_pawnFreeMovements :: Board -> Color -> Position -> [Position]
_pawnFreeMovements board turn (x, y) =
  let unboundedNormalPositions = takeWhile (isSquareFree board) $ concat $ allMovements pawnPiece (x, y)
      unboundedCapturablePositions = filter (isPositionOccupiedByEnemy board turn) $ filter isInsideBoard [(x + forwardDir, y - 1), (x + forwardDir, y + 1)]
   in unboundedNormalPositions ++ unboundedCapturablePositions
  where
    forwardDir = if isColor White pawnPiece then -1 else 1
    selectedSquare = square board (x, y)
    pawnPiece = piece selectedSquare

_generalFreeMovements :: Board -> Color -> Position -> [Position]
_generalFreeMovements board turn position = concatMap (availabilityPositionFilter board turn) (allMovements colouredPiece position)
  where
    selectedSquare = square board position
    colouredPiece = piece selectedSquare

pieceUnboundedMovements :: CPiece -> Position -> [[Position]]
pieceUnboundedMovements (CPiece (Black, Pawn)) = bPawnMovements
pieceUnboundedMovements (CPiece (White, Pawn)) = wPawnMovements
pieceUnboundedMovements (CPiece (_, Rook)) = rookMovements
pieceUnboundedMovements (CPiece (_, Knight)) = knightMovements
pieceUnboundedMovements (CPiece (_, Bishop)) = bishopMovements
pieceUnboundedMovements (CPiece (_, Queen)) = queenMovements
pieceUnboundedMovements (CPiece (_, King)) = kingMovements

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
