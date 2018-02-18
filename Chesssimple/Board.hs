{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Chesssimple.Board (
  Board,
  Position,
  Square(BlankSquare,OccupiedSquare),
  Piece(Pawn, Tower, Knight, Bishop, Queen, King),
  ColouredPiece(CP),
  newBoard, newBoardFromList, classicBoard, legalGrab, freeMovements, possibleMovementsForeachTeamPosition, teamMovements,
  enemyMovements, allMovements, movePiece, isCheck, isCheckMate, placePiece,
  isValid, piece, square, isSquareFree, isSquareOccupiedByEnemy, positionsOf) where

import Chesssimple.Color

import Data.Matrix
import qualified Data.List as List(intersect)
import Control.DeepSeq
import GHC.Generics (Generic)

type Board         = Matrix Square
type Position      = (Int, Int)
data Square        = BlankSquare | OccupiedSquare ColouredPiece deriving (Eq, Generic, NFData)
data Piece         = Pawn | Knight | Bishop | Tower | Queen | King deriving (Eq, Generic, NFData)
data ColouredPiece = CP (Color, Piece) deriving (Eq, Generic, NFData)

instance Show Square where
  show BlankSquare         = "_"
  show (OccupiedSquare cp) = show cp

instance Show ColouredPiece where
  show (CP (color, piece)) = show color ++ show piece

instance Show Piece where
  show Pawn   = "p"
  show Knight = "N"
  show Bishop = "B"
  show Tower  = "T"
  show Queen  = "Q"
  show King   = "K"

newBoard :: (Position -> Square) -> Board
newBoard = matrix 8 8

newBoardFromList :: [Square] -> Board
newBoardFromList sqrs = fromList 8 8 sqrs

----------------
-- Board Layouts
----------------
classicBoard :: Board
classicBoard = newBoard classicLayout

classicLayout :: Position -> Square
classicLayout (i, j)
  | (i == 1) && (j == 1 || j == 8) = OccupiedSquare $ CP (Black, Tower)
  | (i == 1) && (j == 2 || j == 7) = OccupiedSquare $ CP (Black, Knight)
  | (i == 1) && (j == 3 || j == 6) = OccupiedSquare $ CP (Black, Bishop)
  | (i == 1) && (j == 4)           = OccupiedSquare $ CP (Black, Queen)
  | (i == 1) && (j == 5)           = OccupiedSquare $ CP (Black, King)
  | (i == 2)                       = OccupiedSquare $ CP (Black, Pawn)
  | (i == 8) && (j == 1 || j == 8) = OccupiedSquare $ CP (White, Tower)
  | (i == 8) && (j == 2 || j == 7) = OccupiedSquare $ CP (White, Knight)
  | (i == 8) && (j == 3 || j == 6) = OccupiedSquare $ CP (White, Bishop)
  | (i == 8) && (j == 4)           = OccupiedSquare $ CP (White, Queen)
  | (i == 8) && (j == 5)           = OccupiedSquare $ CP (White, King)
  | (i == 7)                       = OccupiedSquare $ CP (White, Pawn)
  | otherwise                      = BlankSquare

-- Checks if one movement can be performed according
-- the turn (color) and the position
legalGrab :: Board -> Color -> Position -> Bool
legalGrab board turn position = let square = safeGet (fst position) (snd position) board
                                 in case square of
                                      Just BlankSquare -> False
                                      Just square      -> not $ isSquareOccupiedByEnemy square turn
                                      Nothing          -> False

movePiece :: Board -> Color -> Position -> Position -> Maybe Board
movePiece board turn src dst = let availableMovements = freeMovements board turn src
                                in if dst `elem` availableMovements
                                   then Just $ treatPromotions $ placePiece board src dst
                                   else Nothing

treatPromotions :: Board -> Board
treatPromotions board = mapRow (promotePawns White) 1 $ mapRow (promotePawns Black) 8 board
  where promotePawns color = \_ square -> if isSquareOccupiedByPiece color Pawn square then OccupiedSquare (CP (color, Queen))
                                                                                       else square

placePiece :: Board -> Position -> Position -> Board
placePiece board positionFrom positionTo =
  let srcSquare = board ! positionFrom
   in setElem BlankSquare positionFrom (setElem srcSquare positionTo board)

isCheck :: Board -> Color -> Bool
isCheck board color = isPositionThreatened board color $ head $ positionsOf board color King

isCheckMate :: Board -> Color -> Bool
isCheckMate board color = isCheck board color && (null $ teamMovements board color)

isValid :: Board -> Bool
isValid board = let blackKingPos = head $ positionsOf board Black King
                    whiteKingPos = head $ positionsOf board White King
                 in not (isPositionThreatened board White blackKingPos || isPositionThreatened board Black whiteKingPos)

isPositionThreatened :: Board -> Color -> Position -> Bool
isPositionThreatened board color position = position `elem` enemyMovements board color

validChessPositions :: [Position]
validChessPositions = [ (x,y) | x <- [1..8], y <- [1..8] ]

-- A Position list of the desired pieces (color & piece type)
positionsOf :: Board -> Color -> Piece -> [Position]
positionsOf board color somePiece = let occupiedPositions = filter (not.isSquareFree board) validChessPositions
                                        piecePred = \position -> (CP (color, somePiece)) == (piece $ square board position)
                                     in filter piecePred occupiedPositions

-- in this function there is control if the kings position is in check or not.
freeMovements :: Board -> Color -> Position -> [Position]
freeMovements board turn position =
  filter (\possibleDst -> not $ isCheck (placePiece board position possibleDst) turn) $ uncheckedFreeMovements board turn position

-- 'unchecked' means that there is no control if the kings position is in check or not.
uncheckedFreeMovements :: Board -> Color -> Position -> [Position]
uncheckedFreeMovements board turn position
  | not (legalGrab board turn position) = []
  | isKnightOccupied board position     = _knightFreeMovements  board turn position
  | isPawnOccupied board position       = _pawnFreeMovements    board turn position
  | otherwise                           = _generalFreeMovements board turn position

teamPositions :: Board -> Color -> [Position]
teamPositions board color = [ position | position <- validChessPositions, isSquareOccupiedBy color $ square board position ]

possibleMovementsForeachTeamPosition :: Board -> Color -> [(Position, [Position])]
possibleMovementsForeachTeamPosition board color = map (\src -> (src, freeMovements board color src)) (teamPositions board color)

teamMovements :: Board -> Color -> [Position]
teamMovements board color = concatMap (freeMovements board color) (teamPositions board color)

enemyMovements :: Board -> Color -> [Position]
enemyMovements board color =
  let enemyColor = switch color
   in concatMap (uncheckedFreeMovements board enemyColor) (teamPositions board enemyColor)

availabilityPositionFilter :: Board -> Color -> [Position] -> [Position]
availabilityPositionFilter board turn positions = let (frees, occupied) = span (isSquareFree board) positions
                                                   in if (not . null) occupied && isPositionOccupiedByEnemy board turn (head occupied)
                                                      then (head occupied):frees
                                                      else frees

isPositionOccupiedByEnemy :: Board -> Color -> Position -> Bool
isPositionOccupiedByEnemy board color position = isSquareOccupiedByEnemy (square board position) color

allMovements :: ColouredPiece -> Position -> [[Position]]
allMovements colouredPiece position = map (filter isInsideBoard) $ pieceUnboundedMovements colouredPiece position

isSquareFree :: Board -> Position -> Bool
isSquareFree board position = square board position == BlankSquare

square :: Board -> Position -> Square
square board position = board ! position

piece :: Square -> ColouredPiece
piece (OccupiedSquare colorPiece) = colorPiece

color :: Square -> Maybe Color
color (OccupiedSquare (CP (color, _))) = Just color
color BlankSquare                      = Nothing

isSquareOccupiedBy :: Color -> Square -> Bool
isSquareOccupiedBy color BlankSquare = False
isSquareOccupiedBy color square      = isColor color $ piece square

isSquareOccupiedByPiece :: Color -> Piece -> Square -> Bool
isSquareOccupiedByPiece color piece BlankSquare                = False
isSquareOccupiedByPiece color piece (OccupiedSquare (CP (c, p))) = color == c && piece == p

isSquareOccupiedByEnemy :: Square -> Color -> Bool
isSquareOccupiedByEnemy (OccupiedSquare (CP (Black, _))) White = True
isSquareOccupiedByEnemy (OccupiedSquare (CP (White, _))) Black = True
isSquareOccupiedByEnemy _ _                                    = False

isInsideBoard :: Position -> Bool
isInsideBoard = \(i,j) -> i >= 1 && i <= 8 && j >= 1 && j <= 8

isKingOccupied :: Board -> Position -> Bool
isKingOccupied board position = isOccupiedBy Black King board position || isOccupiedBy White King board position

isKnightOccupied :: Board -> Position -> Bool
isKnightOccupied board position = isOccupiedBy Black Knight board position || isOccupiedBy White Knight board position

isPawnOccupied :: Board -> Position -> Bool
isPawnOccupied board position = isOccupiedBy Black Pawn board position || isOccupiedBy White Pawn board position

isOccupiedBy :: Color -> Piece -> Board -> Position -> Bool
isOccupiedBy color piece board position = let colouredPiece = square board position
                               in colouredPiece == OccupiedSquare (CP (color, piece))

isColor :: Color -> ColouredPiece -> Bool
isColor color (CP (colorPiece, _)) = colorPiece == color

---------
--Private
---------

_knightFreeMovements :: Board -> Color -> Position -> [Position]
_knightFreeMovements board turn position =
  let isOccupiedByMe = \dstPos -> isSquareOccupiedBy turn $ square board dstPos
   in concatMap (filter (\dstPos -> not $ isOccupiedByMe dstPos)) (allMovements colouredPiece position)
  where
    selectedSquare = square board position
    colouredPiece  = piece selectedSquare

_pawnFreeMovements :: Board -> Color -> Position -> [Position]
_pawnFreeMovements board turn (x,y) =
  let unboundedNormalPositions     = takeWhile (isSquareFree board) $ concat $ allMovements pawnPiece (x,y)
      unboundedCapturablePositions = filter (isPositionOccupiedByEnemy board turn) $ filter isInsideBoard $ [(x+forwardDir, y-1), (x+forwardDir, y+1)]
   in unboundedNormalPositions ++ unboundedCapturablePositions
  where
    forwardDir     = if (isColor White pawnPiece) then -1 else 1
    selectedSquare = square board (x,y)
    pawnPiece      = piece selectedSquare

_generalFreeMovements :: Board -> Color -> Position -> [Position]
_generalFreeMovements board turn position = concatMap (availabilityPositionFilter board turn) (allMovements colouredPiece position)
  where
    selectedSquare = square board position
    colouredPiece  = piece selectedSquare

pieceUnboundedMovements :: ColouredPiece -> Position -> [[Position]]
pieceUnboundedMovements (CP (Black, Pawn)) = blackPawnMovements
pieceUnboundedMovements (CP (White, Pawn)) = whitePawnMovements
pieceUnboundedMovements (CP (_, Tower))    = towerMovements
pieceUnboundedMovements (CP (_, Knight))   = knightMovements
pieceUnboundedMovements (CP (_, Bishop))   = bishopMovements
pieceUnboundedMovements (CP (_, Queen))    = queenMovements
pieceUnboundedMovements (CP (_, King))     = kingMovements

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
kingMovements (i,j) = let firstSteps = \positions -> if null positions then [] else [head positions]
                       in map firstSteps $ queenMovements (i,j)

knightMovements :: Position -> [[Position]]
knightMovements (i,j) = let factors = [ (x,y) | x <- [-2,-1,1,2], y <- [-2,-1,1,2], abs x + abs y == 3 ]
                         in [ [(i + x, j + y)] | (x, y) <- factors ]
