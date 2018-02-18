module Chesssimple.GameAI ( ZeroSumGame, evaluateGame, nextGames, performMovement ) where

import Data.List (minimumBy, maximumBy, sortBy)
import Data.Ord  (compare)
import Data.Tree
import Control.Parallel.Strategies

class (NFData zsg) => ZeroSumGame zsg where
 -- IMPORTANT: 'evaluateGame' function MUST return a score relative to the side to being evaluated.
 -- ie: a positive number is a game favourable to the player that currently needs to move.
  evaluateGame  :: zsg -> Int
  nextGames     :: zsg -> [zsg]

performMovement :: (ZeroSumGame zsg) => zsg -> Integer -> zsg
performMovement game strength = bestGame (allPossibleGames `using` parList rdeepseq)
  where bestGame         = fst.minimumBy (\(_,score1) (_,score2) -> compare score1 score2)
        allPossibleGames = [(possibleGame, computeNegamax strength possibleGame) | possibleGame <- nextGames game]

computeNegamax :: (ZeroSumGame zsg) => Integer -> zsg -> Int
computeNegamax depth game = negamax $ fmap evaluateGame $ (choptree depth) $ gametree game

negamax :: (Tree Int) -> Int
negamax = maximum.negamax'

negamax' :: (Tree Int) -> [Int]
negamax' (Node x []) = [x]
negamax' (Node _ lt) = fmap negate $ mapmax (fmap negamax' lt)

mapmax :: [[Int]] -> [Int]
mapmax []       = []
mapmax (xs:xss) = let max = maximum xs
                   in max:(prune max xss)

prune :: Int -> [[Int]] -> [Int]
prune bound [] = []
prune bound (xs:xss)
  | isOutsideBound xs bound = prune bound xss
  | otherwise               = mapmax (xs:xss)

isOutsideBound :: [Int] -> Int -> Bool
isOutsideBound [] _         = False
isOutsideBound (x:xs) bound = if bound <= x then True else isOutsideBound xs bound

gametree :: (ZeroSumGame zsg) => zsg -> Tree zsg
gametree initial = reptree nextGames initial

-- Auxiliary function that yields a infinite tree from a function
-- and an initial value or seed
reptree :: (a -> [a]) -> a -> Tree a
reptree f initial = Node initial (fmap (reptree f) (f initial))

-- Auxiliary function that cuts-off the tree to a certain level n
choptree :: Integer -> Tree a -> Tree a
choptree 0 (Node a _ ) = Node a []
choptree n (Node a lt) = Node a (fmap (choptree (n - 1)) lt)
