module Chesssimple.Board (
  Board,
  Position,
  ColouredPiece(Black,White),
  Piece(Pawn, Tower, Knight, Bishop, Queen, King),
  classicBoard, freeMovements, allMovements ) where

import Data.Matrix

type Board  = Matrix Square

type Position = (Int, Int)

data Square = BlankSquare | OccupiedSquare ColouredPiece deriving (Eq)

data ColouredPiece = Black Piece | White Piece deriving (Eq)

data Piece = Pawn | Knight | Bishop | Tower | Queen | King deriving (Eq)

instance Show Square where
  show BlankSquare         = "."
  show (OccupiedSquare cp) = show cp

instance Show ColouredPiece where
  show (Black p) = "b" ++ show p
  show (White p) = "w" ++ show p

instance Show Piece where
  show Pawn   = "P"
  show Knight = "N"
  show Bishop = "B"
  show Tower  = "T"
  show Queen  = "Q"
  show King   = "K"

newBoard :: (Position -> Square) -> Board
newBoard = matrix 8 8

classicBoard :: Board
classicBoard = newBoard classicLayout

classicLayout :: Position -> Square
classicLayout (i, j)
  | (i == 1) && (j == 1 || j == 8) = OccupiedSquare $ Black Tower
  | (i == 1) && (j == 2 || j == 7) = OccupiedSquare $ Black Knight
  | (i == 1) && (j == 3 || j == 6) = OccupiedSquare $ Black Bishop
  | (i == 1) && (j == 4)           = OccupiedSquare $ Black Queen
  | (i == 1) && (j == 5)           = OccupiedSquare $ Black King
  | (i == 2)                       = OccupiedSquare $ Black Pawn
  | (i == 8) && (j == 1 || j == 8) = OccupiedSquare $ White Tower
  | (i == 8) && (j == 2 || j == 7) = OccupiedSquare $ White Knight
  | (i == 8) && (j == 3 || j == 6) = OccupiedSquare $ White Bishop
  | (i == 8) && (j == 4)           = OccupiedSquare $ White Queen
  | (i == 8) && (j == 5)           = OccupiedSquare $ White King
  | (i == 7)                       = OccupiedSquare $ White Pawn
  | otherwise                      = BlankSquare

fischerLayout :: Position -> Square
fischerLayout (i, j) = BlankSquare -- TODO

freeMovements :: Board -> Position -> [Position]
freeMovements board position
  | isSquareFree     board position = []
  | isKnightOccupped board position = concatMap (filter (isSquareFree board)) (allMovements colouredPiece position)
  | otherwise                       = concatMap (takeWhile (isSquareFree board)) (allMovements colouredPiece position)
  where
    colouredPiece = piece (square board position)

allMovements :: ColouredPiece -> Position -> [[Position]]
allMovements colouredPiece position = map (filter isInsideBoard) $ pieceUnboundedMovements colouredPiece position

isSquareFree :: Board -> Position -> Bool
isSquareFree board position = square board position == BlankSquare

square :: Board -> Position -> Square
square board position = board ! position

piece :: Square -> ColouredPiece
piece (OccupiedSquare colorPiece) = colorPiece

isInsideBoard :: Position -> Bool
isInsideBoard = \(i,j) -> i >= 1 && i <= 8 && j >= 1 && j <= 8

isKnightOccupped :: Board -> Position -> Bool
isKnightOccupped board position = let colouredPiece = square board position
                                   in colouredPiece == OccupiedSquare (Black Knight) ||
                                      colouredPiece == OccupiedSquare (White Knight)

---------
--Private
---------

pieceUnboundedMovements :: ColouredPiece -> Position -> [[Position]]
pieceUnboundedMovements (Black Pawn)   = blackPawnMovements
pieceUnboundedMovements (White Pawn)   = whitePawnMovements
pieceUnboundedMovements (Black Tower)  = towerMovements
pieceUnboundedMovements (White Tower)  = towerMovements
pieceUnboundedMovements (Black Knight) = knightMovements
pieceUnboundedMovements (White Knight) = knightMovements
pieceUnboundedMovements (Black Bishop) = bishopMovements
pieceUnboundedMovements (White Bishop) = bishopMovements
pieceUnboundedMovements (Black Queen)  = queenMovements
pieceUnboundedMovements (White Queen)  = queenMovements
pieceUnboundedMovements (Black King)   = kingMovements
pieceUnboundedMovements (White King)   = kingMovements

-- Specific Piece Movements

blackPawnMovements :: Position -> [[Position]]
blackPawnMovements (i,j) = if i == 2 then [[(i+1, j), (i+2, j)]]
                                     else [[(i+1, j)]]

whitePawnMovements :: Position -> [[Position]]
whitePawnMovements (i,j) = if i == 7 then [[(i-1, j), (i-2, j)]]
                                     else [[(i-1, j)]]

towerMovements :: Position -> [[Position]]
towerMovements (i,j) = let up    = [ (x, y) | x <- [i], y <- [j+1..8] ]
                           down  = [ (x, y) | x <- [i], y <- [j-1..1] ]
                           left  = [ (x, y) | x <- [i-1..1], y <- [j] ]
                           right = [ (x, y) | x <- [i+1..8], y <- [j] ]
                        in [ up, down, left, right ]

bishopMovements :: Position -> [[Position]]
bishopMovements (i,j) = let upperLeft  = [ (x, y) | x <- [i-1..1], y <- [j+1..8] ]
                            upperRight = [ (x, y) | x <- [i+1..1], y <- [j+1..8] ]
                            lowerLeft  = [ (x, y) | x <- [i-1..1], y <- [j-1..8] ]
                            lowerRight = [ (x, y) | x <- [i+1..1], y <- [j-1..8] ]
                         in [ upperLeft, upperRight, lowerLeft, lowerRight]

queenMovements :: Position -> [[Position]]
queenMovements (i,j) = bishopMovements (i,j) ++ towerMovements (i,j)

kingMovements :: Position -> [[Position]]
kingMovements (i,j) = let firstSteps = \positions -> if null positions
                          then []
                          else [head positions]
                       in map firstSteps $ queenMovements (i,j)

knightMovements :: Position -> [[Position]]
knightMovements (i,j) = let factors = [ (x,y) | x <- [-2,-1,1,2], y <- [-2,-1,1,2], abs x + abs y == 3 ]
                         in [ [(i + x, j + y)] | (x, y) <- factors ]
