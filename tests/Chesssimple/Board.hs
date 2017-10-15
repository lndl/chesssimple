import Test.QuickCheck
import Chesssimple.Board

instance Arbitrary ColouredPiece where
  arbitrary = defaultArbitraryForColouredPiece

defaultArbitraryForColouredPiece :: Gen ColouredPiece
defaultArbitraryForColouredPiece  = do
  color <- elements [Black, White]
  piece <- elements [Pawn, Tower, Knight, Bishop, Queen, King]
  return (color piece)

prop_queen_movs :: Position -> Bool
prop_queen_movs xy =
  (pieceAllBoardMovements (White Queen) xy == pieceAllBoardMovements (White Bishop) xy ++ pieceAllBoardMovements (White Tower) xy) &&
  (pieceAllBoardMovements (Black Queen) xy == pieceAllBoardMovements (Black Bishop) xy ++ pieceAllBoardMovements (Black Tower) xy)

prop_all_pieces_inside_board :: ColouredPiece -> Position -> Bool
prop_all_pieces_inside_board cpiece pos =
  all isInsideBoard (pieceAllBoardMovements cpiece pos)
