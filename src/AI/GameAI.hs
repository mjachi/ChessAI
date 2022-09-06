module AI.GameAI (ZeroSumGame, evaluateGame, nextGames, performMovement) where

import Control.Parallel.Strategies
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Ord (compare)
import Data.Tree

class (NFData zsg) => ZeroSumGame zsg where
  -- 'evaluateGame' function MUST return a score relative to the side to being evaluated.
  -- ie: a positive number is a game favourable to the player that currently needs to move.
  evaluateGame :: zsg -> Int
  nextGames :: zsg -> [zsg]

performMovement :: (ZeroSumGame zsg) => zsg -> Integer -> zsg
performMovement game strength = bestGame (allPossibleGames `using` parList rdeepseq)
  where
    bestGame = fst . minimumBy (\(_, score1) (_, score2) -> compare score1 score2)
    allPossibleGames = [(possibleGame, computeNegamax strength possibleGame) | possibleGame <- nextGames game]

computeNegamax :: (ZeroSumGame zsg) => Integer -> zsg -> Int
computeNegamax depth game = negamax $ fmap evaluateGame $ pruneTree depth $ gametree game

negamax :: Tree Int -> Int
negamax = maximum . negamax'

negamax' :: Tree Int -> [Int]
negamax' (Node x []) = [x]
negamax' (Node _ lt) = negate <$> mapmax (fmap negamax' lt)

mapmax :: [[Int]] -> [Int]
mapmax [] = []
mapmax (xs : xss) =
  let mmax = maximum xs
   in mmax : prune mmax xss

prune :: Int -> [[Int]] -> [Int]
prune _ [] = []
prune bound (xs : xss)
  | isOutsideBound xs bound = prune bound xss
  | otherwise = mapmax (xs : xss)

isOutsideBound :: [Int] -> Int -> Bool
isOutsideBound [] _ = False
isOutsideBound (x : xs) bound = (bound <= x) || isOutsideBound xs bound

-- | Defines game tree given zero sum game instance
gametree :: (ZeroSumGame zsg) => zsg -> Tree zsg
gametree = reptree nextGames

-- | Yields a infinite tree from a function and an initial value or seed
reptree :: (a -> [a]) -> a -> Tree a
reptree f initial = Node initial (fmap (reptree f) (f initial))

-- | Cuts off the tree to a certain level n
pruneTree :: Integer -> Tree a -> Tree a
pruneTree 0 (Node a _) = Node a []
pruneTree n (Node a lt) = Node a (fmap (pruneTree (n - 1)) lt)
