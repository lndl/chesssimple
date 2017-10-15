module Chesssimple.Board (
  Board,
  Position,
  ColouredPiece(Black,White),
  Piece(Pawn, Tower, Knight, Bishop, Queen, King),
  isInsideBounds, isInsideBoard, pieceAllBoardMovements, newBoard, classicLayout ) where

import Data.Matrix

type Board  = Matrix Square

type Position = (Int, Int)

data Square = BlankSquare | OccupiedSquare ColouredPiece deriving (Eq, Show)

data ColouredPiece = Black Piece | White Piece deriving (Eq, Show)

data Piece = Pawn | Knight | Bishop | Tower | Queen | King deriving (Eq, Show)

newBoard :: (Position -> Square) -> Board
newBoard = matrix 8 8

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
??????

pieceAllBoardMovements :: ColouredPiece -> Position -> [Position]
pieceAllBoardMovements colorPiece position = filter isInsideBounds $ pieceUnboundedMovements colorPiece position

isInsideBoard :: Position -> Bool
isInsideBoard = isInsideBounds

isInsideBounds :: Position -> Bool
isInsideBounds = \(i,j) -> i >= 1 && i <= 8 && j >= 1 && j <= 8

isSquareFree :: Board -> Position -> Bool
isSquareFree board position = square board position == BlankSquare

isKnightOccupped :: Board -> Position -> Bool
isKnightOccupped board position = let colouredPiece = square board position
                                   in colouredPiece == OccupiedSquare (Black Knight) ||
                                      colouredPiece == OccupiedSquare (White Knight)

square :: Board -> Position -> Square
square board position = board ! position

piece :: Square -> ColouredPiece
piece (OccupiedSquare colorPiece) = colorPiece

---------
--Private
---------

pieceUnboundedMovements :: ColouredPiece -> Position -> [Position]
pieceUnboundedMovements (Black Pawn)   = pawnMovements 1
pieceUnboundedMovements (White Pawn)   = pawnMovements (-1)
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

pawnMovements :: Int -> Position -> [Position]
pawnMovements factor (i,j) = [ (i + factor, j) ]

towerMovements :: Position -> [Position]
towerMovements (i,j) = let vertical   = [ (x, j) | x <- [1..8], x /= i ]
                           horizontal = [ (i, y) | y <- [1..8], y /= j ]
                        in vertical ++ horizontal

knightMovements :: Position -> [Position]
knightMovements (i,j) = let factors = [ (x,y) | x <- [-2,-1,1,2], y <- [-2,-1,1,2], abs x + abs y == 3 ]
                         in [ (i + x, j + y) | (x, y) <- factors ]

bishopMovements :: Position -> [Position]
bishopMovements (i,j) = let upLeftRight = [ (x, y) | x <- [1..8], y <- [1..8], x + y == i + j, x /= i, y /= j ]
                            upRightLeft = [ (x, y) | x <- [1..8], y <- [1..8], x - y == i - j, x /= i, y /= j ]
                         in upLeftRight ++ upRightLeft

queenMovements :: Position -> [Position]
queenMovements (i,j) = bishopMovements (i,j) ++ towerMovements (i,j)

kingMovements :: Position -> [Position]
kingMovements (i,j) = let factors = [ (x,y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0 ]
                      in [ (i + x, j + y) | (x, y) <- factors ]
