module Chesssimple.GameAI ( ZeroSumGame, evaluateGame, nextGames, isGameOver, performMovement ) where

import Data.List (minimumBy)
import Data.Ord  (compare)
import Data.Tree

class ZeroSumGame zsg where
 -- IMPORTANT: 'evaluateGame' function MUST return a score relative to the side to being evaluated.
 -- ie: a positive number is a game favourable to the player that currently needs to move.
  evaluateGame  :: zsg -> Int
  nextGames     :: zsg -> [zsg]
  isGameOver    :: zsg -> Bool

performMovement :: (ZeroSumGame zsg) => zsg -> Integer -> zsg
performMovement game strength = bestGame $ [(possibleGame, computeMimimax strength possibleGame) | possibleGame <- bestNextGames game]
  where bestGame = fst.minimumBy (\(_,score1) (_,score2) -> compare score1 score2)

computeMimimax :: (ZeroSumGame zsg) => Integer -> zsg -> Int
computeMimimax depth = minimax.fmap evaluateGame.(choptree depth).gametree

bestNextGames :: (ZeroSumGame zsg) => zsg -> [zsg]
bestNextGames game = nextGames game

minimax :: (Tree Int) -> Int
minimax (Node x []) = x
minimax (Node _ lt) = negate.minimum.(fmap minimax) $ lt

gametree :: (ZeroSumGame zsg) => zsg -> Tree zsg
gametree initial = reptree bestNextGames initial

-- Auxiliary function that yields a infinite tree from a function
-- and an initial value or seed
reptree :: (a -> [a]) -> a -> Tree a
reptree f initial = Node initial (fmap (reptree f) (f initial))

-- Auxiliary function that cuts-off the tree to a certain level n
choptree :: Integer -> Tree a -> Tree a
choptree 0 (Node a _ ) = Node a []
choptree n (Node a lt) = Node a (fmap (choptree (n - 1)) lt)
