{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Chesssimple.Game (
  Game, new, isInitial, isCheck, isCheckMate, tryMovement, currentBoard,
  whoPlaysNow, update, turn, show, availableMovements, undo, player1, player2 ) where

import qualified Chesssimple.Player as Player
import qualified Chesssimple.Board  as Board
import qualified Chesssimple.Color  as Color
import qualified Chesssimple.GameAI as GameAI
import qualified Chesssimple.BoardAI as BoardAI

import qualified Data.List as List
import Data.Maybe (catMaybes)
import Control.DeepSeq
import GHC.Generics (Generic)

data Game = Game { player1 :: Player.Player
                 , player2 :: Player.Player
                 , plays   :: [Board.Board]
                 , turn    :: Color.Color
                 } deriving (Generic, NFData)

instance Show Game where
  show Game {player1=_, player2=_, plays=plays} = show $ head plays

instance GameAI.ZeroSumGame Game where
  evaluateGame game = BoardAI.evaluateBoard (currentBoard game) (turn game)
  nextGames game =
    let allPossibilities = Board.possibleMovementsForeachTeamPosition (currentBoard game) (turn game)
     in catMaybes $ concatMap (\(src, possibleDsts) -> map (\dst -> tryMovement game src dst) possibleDsts) allPossibilities

new :: Player.Player -> Player.Player -> Game
new p1 p2 = new' p1 p2 [Board.classicBoard] Color.White

update :: Game -> Board.Board -> Game
update game newBoard = let p1       = player1 game
                           p2       = player2 game
                           newPlays = newBoard:(plays game)
                           nextTurn = Color.switch $ turn game
                        in new' p1 p2 newPlays nextTurn

undo :: Game -> Game
undo game = let p1                   = player1 game
                p2                   = player2 game
                (_:_:previousPlays)  = plays game
             in new' p1 p2 previousPlays (turn game)

currentBoard :: Game -> Board.Board
currentBoard game = head $ plays game

whoPlaysNow :: Game -> Player.Player
whoPlaysNow game = if turn game == Color.White then playerWithColor Color.White game
                                               else playerWithColor Color.Black game
  where
    playerWithColor color game = if (Player.color $ player1 game) == color then player1 game
                                                                           else player2 game

isInitial :: Game -> Bool
isInitial game = length (plays game) == 1

isCheckMate :: Game -> Bool
isCheckMate game = Board.isCheckMate (currentBoard game) (turn game)

isCheck :: Game -> Bool
isCheck game = Board.isCheck (currentBoard game) (turn game)

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
