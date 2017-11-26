module Chesssimple.Game ( Game, new, isFinished, tryMovement, turn, show, availableMovements ) where

import qualified Chesssimple.Player as Player
import qualified Chesssimple.Board  as Board
import qualified Chesssimple.Color  as Color

import qualified Data.List as List

data Game = Game { player1 :: Player.Player
                 , player2 :: Player.Player
                 , plays   :: [Board.Board]
                 , turn    :: Color.Color
                 }

instance Show Game where
  show Game {player1=_, player2=_, plays=plays} = show $ head plays

new :: Player.Player -> Player.Player -> Game
new p1 p2 = new' p1 p2 [Board.classicBoard] Color.White

update :: Game -> Board.Board -> Game
update game newBoard = let p1       = player1 game
                           p2       = player2 game
                           newPlays = newBoard:(plays game)
                           nextTurn = Color.switch $ turn game
                        in new' p1 p2 newPlays nextTurn

currentBoard :: Game -> Board.Board
currentBoard game = head $ plays game

isFinished :: Game -> Bool
isFinished game = False

tryMovement :: Game -> Board.Position -> Board.Position -> Maybe Game
tryMovement game src dst = case Board.movePiece (currentBoard game) (turn game) src dst of
                          Just newBoard -> Just (update game newBoard)
                          Nothing       -> Nothing

availableMovements :: Game -> Board.Position -> [Board.Position]
availableMovements game position = Board.freeMovements (currentBoard game) (turn game) position

new' :: Player.Player -> Player.Player -> [Board.Board] -> Color.Color -> Game
new' p1 p2 plays color = Game { player1 = p1
                              , player2 = p2
                              , plays   = plays
                              , turn    = color
                        }
