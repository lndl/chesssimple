import Test.QuickCheck
import Chesssimple.Board
import qualified Data.Set as Set

instance Arbitrary ColouredPiece where
  arbitrary = defaultArbitraryForColouredPiece

defaultArbitraryForColouredPiece :: Gen ColouredPiece
defaultArbitraryForColouredPiece  = do
  color <- elements [Black, White]
  piece <- elements [Pawn, Tower, Knight, Bishop, Queen, King]
  return (color piece)

validChessPosition :: Gen Position
validChessPosition = elements [(x,y) | x <- [1..8], y <- [1..8]]

prop_queen_movs :: Position -> Bool
prop_queen_movs xy =
  (Set.fromList (allMovements (White Queen) xy) == Set.fromList (allMovements (White Bishop) xy ++ allMovements (White Tower) xy)) &&
  (Set.fromList (allMovements (Black Queen) xy) == Set.fromList (allMovements (Black Bishop) xy ++ allMovements (Black Tower) xy))

-- Initial properties from board

prop_initial_pieces_movs :: Property
prop_initial_pieces_movs = forAll validChessPosition checkInitialPiecesMovs

checkInitialPiecesMovs :: Position -> Bool
checkInitialPiecesMovs (x,y)
  | (x == 1 || x == 8) && not isKnight  = (length $ freeMovements classicBoard (x,y)) == 0 -- major pieces expect knights
  | isKnight                            = (length $ freeMovements classicBoard (x,y)) == 2 -- knights only
  | (x == 2 || x == 7)                  = (length $ freeMovements classicBoard (x,y)) == 2 -- pawns only
  | otherwise                           = (length $ freeMovements classicBoard (x,y)) == 0 -- blanks
  where isKnight = elem (x,y) [(1,2),(1,7),(8,2),(8,7)]
