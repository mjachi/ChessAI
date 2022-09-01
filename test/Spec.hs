import Chess.Board
import Test.QuickCheck

-----------------------------------------------------------------------------------------
-- GENERATORS
-----------------------------------------------------------------------------------------

validChessPosition :: Gen Position
validChessPosition = elements [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
