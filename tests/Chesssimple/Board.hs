import Test.QuickCheck
import Chesssimple.Board
import Chesssimple.Color
import qualified Data.Set  as Set
import qualified Data.List as List
import Control.Monad

------------------------------------
-- Generators (defaults and customs)
------------------------------------

validChessPosition :: Gen Position
validChessPosition = elements [(x,y) | x <- [1..8], y <- [1..8]]

colors :: Gen Color
colors = elements [Black, White]

instance Arbitrary Color where
  arbitrary = colors

instance Arbitrary Piece where
  arbitrary = elements [Pawn, Tower, Knight, Bishop, Queen, King]

instance Arbitrary ColouredPiece where
  arbitrary = do
    color <- arbitrary
    piece <- arbitrary
    return (CP (color, piece))

instance Arbitrary Square where
  arbitrary = do
    colouredPiece <- arbitrary
    square <- elements [BlankSquare, OccupiedSquare colouredPiece]
    return square

-- a random board is anyone who any number and types of pieces,
-- except for Kings: there are always only one black king and only one white king
randomBoard :: Gen Board
randomBoard =
  let onlyOneKing   = \color squares -> length (List.elemIndices (OccupiedSquare (CP (color, King))) squares) == 1
      fullGenCond   = \squares -> (onlyOneKing White squares) && (onlyOneKing Black squares)
    in do
      randomSquares <- suchThat (vectorOf 64 (arbitrary :: Gen Square)) fullGenCond
      return $ newBoardFromList randomSquares

validBoard :: Gen Board
validBoard = suchThat randomBoard isValid

------------------------------------
-- Properties
------------------------------------

prop_queen_movs :: Position -> Bool
prop_queen_movs xy =
  (Set.fromList (allMovements (CP (White, Queen)) xy) == Set.fromList (allMovements (CP (White, Bishop)) xy ++ allMovements (CP (White, Tower)) xy)) &&
    (Set.fromList (allMovements (CP (Black, Queen)) xy) == Set.fromList (allMovements (CP (Black, Bishop)) xy ++ allMovements (CP (Black, Tower)) xy))



------------------------
-- Properties from board
------------------------

-- Checks the very first movements for white band
prop_initial_pieces_movs = forAll validChessPosition checkInitialPiecesMovs

checkInitialPiecesMovs :: Position -> Bool
checkInitialPiecesMovs (x,y)  -- keep in mind that initial check of chess pieces are in thw white turn
  | (x == 8) && isKnight      = (length $ freeMovements classicBoard White (x,y)) == 2 -- white knights only
  | (x == 7)                  = (length $ freeMovements classicBoard White (x,y)) == 2 -- white pawns only
  | otherwise                 = (length $ freeMovements classicBoard White (x,y)) == 0 -- the rest of the pieces
  where isKnight = elem (x,y) [(1,2),(1,7),(8,2),(8,7)]


-- Must be always one king per band
prop_always_one_king =
  forAll randomBoard $ \board ->
    forAll (arbitrary :: Gen Color) $ \color ->
      (length $ positionsOf board color King) == 1

-- Must be impossible for a king to move to a position threatened by an enemy
prop_legal_king_movement =
  forAll validBoard $ \board ->
    forAll (arbitrary :: Gen Color) $ \color ->
      let kingPosition = head $ positionsOf board color King
       in let theKingMovements  = Set.fromList (freeMovements  board color kingPosition)
              theEnemyMovements = Set.fromList (enemyMovements board color)
           in null $ Set.intersection theKingMovements theEnemyMovements

-- Must be impossible to initiate a movement over blank squares or enemy pieces
prop_legal_grab =
  forAll randomBoard $ \board ->
    forAll (arbitrary :: Gen Color) $ \color ->
      forAll validChessPosition $ \position ->
        let currentSquare = square board position
         in ((currentSquare == BlankSquare) || (isSquareOccupiedByEnemy currentSquare color)) ==>
          not $ legalGrab board color position
