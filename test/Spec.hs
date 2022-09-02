import Chess.Board
import Test.QuickCheck

-----------------------------------------------------------------------------------------
-- GENERATORS
-----------------------------------------------------------------------------------------

-- | Generator coordinates denoting valid chess positions
validChessPosition :: Gen Position
validChessPosition = elements [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
