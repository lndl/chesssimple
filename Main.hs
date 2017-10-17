module Main where

import qualified Chesssimple.Game   as Game
import qualified Chesssimple.Player as Player
import Chesssimple.Board (Position)
import Chesssimple.Color

import Data.Char

main :: IO ()
main = do
  let player1 = Player.new "Lautaro"
      player2 = Player.new "Sabrina"
      game    = Game.new player1 player2
   in do
     putStrLn "Welcome to Simple Chess Game. Type a move or 'exit'"
     performGameTurn game

performGameTurn :: Game.Game -> IO ()
performGameTurn game
  | Game.finished game = return ()
  | otherwise          = do
      putStrLn $ Game.show game
      putStrLn $ colorTurn game ++ " move? "
      command <- getLine
      case command of
        "exit"    -> return ()
        "avlMovs" -> checkAvailableMovements game
        _ -> performMove game command

parseMove :: String -> Maybe (Position, Position)
parseMove moveStr = let a:b:c:d:_ = map digitToInt (take 4 moveStr)
                     in Just ((a,b),(c,d))

performMove :: Game.Game -> String -> IO ()
performMove game moveStr =
  case parseMove moveStr of
    Just move -> case (Game.tryMovement game move) of
                   Just nextGame -> do
                     performGameTurn nextGame
                   Nothing -> do
                     putStrLn "Illegal movement. Try again."
                     performGameTurn game
    Nothing -> do
      putStrLn "Parse problem in move. Try again."
      performGameTurn game

checkAvailableMovements :: Game.Game -> IO ()
checkAvailableMovements game = return ()

colorTurn :: Game.Game -> String
colorTurn game = case Game.turn game of
                   White -> "White"
                   Black -> "Black"
