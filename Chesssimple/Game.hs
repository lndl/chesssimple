module Chesssimple.Game ( Game , newGame ) where

import Chesssimple.Player
import Chesssimple.Board

data Game = Game { player1 :: Player
                 , player2 :: Player
                 , board   :: Board
                 , turn    :: Int
                 } deriving (Show)


newGame :: Player -> Player -> Game
newGame p1 p2 = Game { player1 = p1
                     , player2 = p2
                     , board   = newBoard classicLayout
                     , turn    = 1
                     }
