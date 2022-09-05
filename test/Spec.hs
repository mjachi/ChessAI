import Chess.Board
import qualified Data.List as List
import Player (Color (Black, White))
import Test.QuickCheck

-----------------------------------------------------------------------------------------
-- GENERATORS
-----------------------------------------------------------------------------------------

-- | Generator coordinates denoting valid chess positions
validPosition :: Gen Position
validPosition = elements [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]

colors :: Gen Color
colors = elements [Black, White]

instance Arbitrary Color where
  arbitrary = colors

instance Arbitrary Piece where
  arbitrary = elements [Pawn, Knight, Bishop, Rook, Queen, King]

instance Arbitrary CPiece where
  arbitrary = do
    cc <- arbitrary
    pp <- arbitrary
    return (CPiece (cc, pp))

instance Arbitrary Square where
  arbitrary = do
    colouredPiece <- arbitrary
    elements [BlankSquare, OccupiedSquare colouredPiece]

randomBoard :: Gen Board
randomBoard =
  let onlyOneKing cc squares =
        length (List.elemIndices (OccupiedSquare (CPiece (cc, King))) squares) == 1
      fullGenCond squares = onlyOneKing White squares && onlyOneKing Black squares
   in do
        randomSquares <- suchThat (vectorOf 64 (arbitrary :: Gen Square)) fullGenCond
        return $ newBoardFromList randomSquares

validBoard :: Gen Board
validBoard = suchThat randomBoard isValid

-----------------------------------------------------------------------------------------
-- MOVE AND BOARD PROPERTIES
-----------------------------------------------------------------------------------------

-- Checks the very first movements for white band
propertyInitialMoves :: Property
propertyInitialMoves = forAll validPosition checkInitialPiecesMovs

checkInitialPiecesMovs :: Position -> Bool
checkInitialPiecesMovs (x, y)
  | (x == 8) && isKnight = length (freeMovements classicBoard White (x, y)) == 2
  | x == 7 = length (freeMovements classicBoard White (x, y)) == 2
  | otherwise = null (freeMovements classicBoard White (x, y))
  where
    isKnight = (x, y) `elem` [(1, 2), (1, 7), (8, 2), (8, 7)]

-- Must be always one king per band
propertyOneKing :: Property
propertyOneKing =
  forAll randomBoard $ \board ->
    forAll (arbitrary :: Gen Color) $ \cc ->
      length (positionsOf board cc King) == 1

-- Must be impossible for a king to move to a position threatened by an enemy
propertyLegalKingMovement :: Property
propertyLegalKingMovement =
  forAll validBoard $ \board ->
    forAll (arbitrary :: Gen Color) $ \cc ->
      let kingPosition = head $ positionsOf board cc King
          theKingMovements = freeMovements board cc kingPosition
          possibleBoards = map (placePiece board kingPosition) theKingMovements
          flippedCheck = flip isCheck
          safeMovement = not <$> flippedCheck cc
       in all safeMovement possibleBoards

-- Must be impossible to initiate a movement over blank squares or enemy pieces
propertyLegalGrab :: Property
propertyLegalGrab =
  forAll randomBoard $ \board ->
    forAll (arbitrary :: Gen Color) $ \cc ->
      forAll validPosition $ \position ->
        let currentSquare = square board position
         in ((currentSquare == BlankSquare) || isSquareOccupiedByEnemy currentSquare cc)
              ==> not
              $ legalGrab board cc position

main :: IO ()
main = do
  quickCheck propertyInitialMoves
  quickCheck propertyLegalKingMovement
  quickCheck propertyLegalGrab
  quickCheck propertyOneKing
