module Chesssimple.Board (
  Board,
  Position,
  Piece(Pawn, Tower, Knight, Bishop, Queen, King),
  ColouredPiece,
  classicBoard, freeMovements, allMovements, movePiece ) where

import Chesssimple.Color

import Data.Matrix

type Board         = Matrix Square
type Position      = (Int, Int)
data Square        = BlankSquare | OccupiedSquare ColouredPiece deriving (Eq)
data Piece         = Pawn | Knight | Bishop | Tower | Queen | King deriving (Eq)
type ColouredPiece = (Color, Piece)

instance Show Square where
  show BlankSquare         = "."
  show (OccupiedSquare cp) = show cp

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
  | (i == 1) && (j == 1 || j == 8) = OccupiedSquare $ (Black, Tower)
  | (i == 1) && (j == 2 || j == 7) = OccupiedSquare $ (Black, Knight)
  | (i == 1) && (j == 3 || j == 6) = OccupiedSquare $ (Black, Bishop)
  | (i == 1) && (j == 4)           = OccupiedSquare $ (Black, Queen)
  | (i == 1) && (j == 5)           = OccupiedSquare $ (Black, King)
  | (i == 2)                       = OccupiedSquare $ (Black, Pawn)
  | (i == 8) && (j == 1 || j == 8) = OccupiedSquare $ (White, Tower)
  | (i == 8) && (j == 2 || j == 7) = OccupiedSquare $ (White, Knight)
  | (i == 8) && (j == 3 || j == 6) = OccupiedSquare $ (White, Bishop)
  | (i == 8) && (j == 4)           = OccupiedSquare $ (White, Queen)
  | (i == 8) && (j == 5)           = OccupiedSquare $ (White, King)
  | (i == 7)                       = OccupiedSquare $ (White, Pawn)
  | otherwise                      = BlankSquare

legalGrab :: Board -> Color -> Position -> Bool
legalGrab board color position = let square = safeGet (fst position) (snd position) board
                                  in case square of
                                    Just (OccupiedSquare (grabbedColor, _)) -> grabbedColor == color
                                    _ -> False

movePiece :: Board -> Color -> Position -> Position -> Maybe Board
movePiece board color src dst =
  if legalGrab board color src
     then let availableMovements = freeMovements board src
           in if dst `elem` availableMovements
                 then let srcSquare = board ! src
                       in Just $ setElem BlankSquare src (setElem srcSquare dst board)
                 else Nothing
     else Nothing

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
                                   in colouredPiece == OccupiedSquare (Black, Knight) ||
                                      colouredPiece == OccupiedSquare (White, Knight)

---------
--Private
---------

pieceUnboundedMovements :: ColouredPiece -> Position -> [[Position]]
pieceUnboundedMovements (Black, Pawn)   = blackPawnMovements
pieceUnboundedMovements (White, Pawn)   = whitePawnMovements
pieceUnboundedMovements (_, Tower)  = towerMovements
pieceUnboundedMovements (_, Knight) = knightMovements
pieceUnboundedMovements (_, Bishop) = bishopMovements
pieceUnboundedMovements (_, Queen)  = queenMovements
pieceUnboundedMovements (_, King)   = kingMovements

-- Specific Piece Movements

blackPawnMovements :: Position -> [[Position]]
blackPawnMovements (i,j) = if i == 2 then [[(i+1, j), (i+2, j)]]
                                     else [[(i+1, j)]]

whitePawnMovements :: Position -> [[Position]]
whitePawnMovements (i,j) = if i == 7 then [[(i-1, j), (i-2, j)]]
                                     else [[(i-1, j)]]

towerMovements :: Position -> [[Position]]
towerMovements (i,j) = let up    = [ (x, y) | x <- reverse [1..i-1], y <- [j] ]
                           down  = [ (x, y) | x <- [i+1..8], y <- [j] ]
                           left  = [ (x, y) | x <- [i], y <- reverse [1..j-1] ]
                           right = [ (x, y) | x <- [i], y <- [j+1..8] ]
                        in [ up, down, left, right ]

bishopMovements :: Position -> [[Position]]
bishopMovements (i,j) = let upperLeft  = zip (reverse [1..i-1]) (reverse [1..j-1])
                            upperRight = zip (reverse [1..i-1]) [j+1..8]
                            lowerLeft  = zip [i+1..8] (reverse [1..j-1])
                            lowerRight = zip [i+1..8] [j+1..8]
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
