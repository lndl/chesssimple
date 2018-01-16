module Chesssimple.BoardAI (evaluateBoard) where

import Data.Matrix
import Chesssimple.Board
import Chesssimple.Color

------------------
-- For AI Scoring
------------------

evaluateBoard :: Board -> Color -> Int
evaluateBoard board color
  | isCheckMate board color = -1000000000 -- NOTE: 'isCheckMate' currently determines if the player is losing by checkmate. Therefore, the board evaluation MUST tend to a infinite negative value, because it's the most unfavourable position for that team.
  | otherwise =
      let numberOf = \color piece -> length $ positionsOf board color piece
          materialScore   = (900 * (numberOf White Queen  - numberOf Black Queen) +
                            500 * (numberOf White Tower  - numberOf Black Tower) +
                            360 * (numberOf White Bishop - numberOf Black Bishop) +
                            320 * (numberOf White Knight - numberOf Black Knight) +
                            100 * (numberOf White Pawn - numberOf Black Pawn))
          positionalScore = positionsScore board White - positionsScore board Black
       in (materialScore + positionalScore) * who2Move
 where who2Move = if color == White then 1 else -1

positionsScore :: Board -> Color -> Int
positionsScore board color = pawnsPositionBonus board color + knightPositionBonus board color + bishopPositionBonus board color + towerPositionBonus board color + queenPositionBonus board color + kingPositionBonus board color

positionsScoreFor :: Board -> Color -> Piece -> [Int] -> Int
positionsScoreFor board color piece scores =
  let boardOrder  = if color == White then id else reverse
      boardScores = fromList 8 8 $ boardOrder scores
   in foldr (+) 0 $ map (\position -> boardScores ! position) (positionsOf board color piece)

pawnsPositionBonus :: Board -> Color -> Int
pawnsPositionBonus board color =
  positionsScoreFor board color Pawn
  [ 0,  0,  0,  0,  0,  0,  0,  0,
   50, 50, 50, 50, 50, 50, 50, 50,
   10, 10, 20, 30, 30, 20, 10, 10,
    5,  5, 10, 25, 25, 10,  5,  5,
    0,  0,  0, 20, 20,  0,  0,  0,
    5, -5, 10,  0,  0,-10, -5,  5,
    5, 10, 10,-20,-20, 10, 10,  5,
    0,  0,  0,  0,  0,  0,  0,  0]

knightPositionBonus :: Board -> Color -> Int
knightPositionBonus board color =
  positionsScoreFor board color Knight
  [-50, -40, -30, -30, -30, -30, -40, -50,
   -40, -20,   0,   0,   0,   0, -20, -40,
   -30,   0,  10,  15,  15,  10,   0, -30,
   -30,   5,  15,  20,  20,  15,   5, -30,
   -30,   0,  15,  20,  20,  15,   0, -30,
   -30,   5,  10,  15,  15,  10,   5, -30,
   -40, -20,   0,   5,   5,   0, -20, -40,
   -50, -40, -30, -30, -30, -30, -40, -50]

bishopPositionBonus :: Board -> Color -> Int
bishopPositionBonus board color =
  positionsScoreFor board color Bishop
  [-20,-10,-10,-10,-10,-10,-10,-20,
   -10,  0,  0,  0,  0,  0,  0,-10,
   -10,  0,  5, 10, 10,  5,  0,-10,
   -10,  5,  5, 10, 10,  5,  5,-10,
   -10,  0, 10, 10, 10, 10,  0,-10,
   -10, 10, 10, 10, 10, 10, 10,-10,
   -10,  5,  0,  0,  0,  0,  5,-10,
   -20,-10,-10,-10,-10,-10,-10,-20]

towerPositionBonus :: Board -> Color -> Int
towerPositionBonus board color =
  positionsScoreFor board color Tower
  [0,  0,  0,  0,  0,  0,  0,  0,
   5, 10, 10, 10, 10, 10, 10,  5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
  -5,  0,  0,  0,  0,  0,  0, -5,
   0,  0,  0,  5,  5,  0,  0,  0]

queenPositionBonus :: Board -> Color -> Int
queenPositionBonus board color =
  positionsScoreFor board color Queen
  [-20,-10,-10, -5, -5,-10,-10,-20,
   -10,  0,  0,  0,  0,  0,  0,-10,
   -10,  0,  5,  5,  5,  5,  0,-10,
    -5,  0,  5,  5,  5,  5,  0, -5,
     0,  0,  5,  5,  5,  5,  0, -5,
   -10,  5,  5,  5,  5,  5,  0,-10,
   -10,  0,  5,  0,  0,  0,  0,-10,
   -20,-10,-10, -5, -5,-10,-10,-20]

kingPositionBonus :: Board -> Color -> Int
kingPositionBonus board color =
  positionsScoreFor board color King
  [-30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -30,-40,-40,-50,-50,-40,-40,-30,
   -20,-30,-30,-40,-40,-30,-30,-20,
   -10,-20,-20,-20,-20,-20,-20,-10,
    20, 20,  0,  0,  0,  0, 20, 20,
    20, 30, 10,  0,  0, 10, 30, 20]
