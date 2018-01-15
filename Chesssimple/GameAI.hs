module Chesssimple.GameAI ( ZeroSumGame, evaluateGame, availableMovements, isGameOver, performMovement ) where

import Data.List (maximumBy)
import Data.Ord  (compare)

class ZeroSumGame zsg where
  -- IMPORTANT: 'evaluateGame' function MUST return a score relative to the side to being evaluated.
  -- ie: a positive number is a game favourable to the player that will perform the move.
  evaluateGame       :: zsg -> Int
  availableMovements :: zsg -> [zsg]
  isGameOver         :: zsg -> Bool

performMovement :: (ZeroSumGame zsg) => zsg -> Integer -> zsg
performMovement game strength = negamax game strength

negamax :: (ZeroSumGame zsg) => zsg -> Integer -> zsg
negamax game depth =
  let negamaxScoreFor   = \aGame -> - negamaxScoreWithAlphaBeta aGame depth (-infinity) infinity
      -- Keep in mind that 'best next games', when the 'evaluateGame' function is applied in each of them, will be evaluated
      -- from the point of view of the opposite band. Therefore, the maximum of the negation of each of those values will be
      -- the worse scenario for the opposite team or, which is the same, the best movement for the current team in play.
      candidateMovs     = fmap (\candidateGame -> (candidateGame, negamaxScoreFor candidateGame)) (bestNextGames game)
      bestMovComparator = \(_, nma) (_, nmb)  -> compare nma nmb
   in fst $ maximumBy bestMovComparator candidateMovs

negamaxScoreWithAlphaBeta :: (ZeroSumGame zsg) => zsg -> Integer ->Int -> Int -> Int
negamaxScoreWithAlphaBeta game depth alpha beta
  | depth == 0 || isGameOver game = evaluateGame game
  | otherwise  = let foldingFunction = \(currentScore, currentA) game -> if currentA >= beta
                     then (currentScore, currentA)
                     else _negamaxPipeline' game depth currentA beta currentScore
                  in fst $ foldl foldingFunction (-infinity, -infinity) (bestNextGames game)

_negamaxPipeline' :: (ZeroSumGame zsg) => zsg -> Integer -> Int -> Int -> Int -> (Int, Int)
_negamaxPipeline' game depth alpha beta currentBestScore =
  let currentScore = - negamaxScoreWithAlphaBeta game (depth - 1) (- beta) (- alpha)
      newBestScore = max currentBestScore currentScore
      newAlpha     = max alpha currentScore
   in (newBestScore, newAlpha)

bestNextGames :: (ZeroSumGame zsg) => zsg -> [zsg]
bestNextGames game = availableMovements game

infinity = 1000000000
