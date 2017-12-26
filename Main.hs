module Main where

import qualified Chesssimple.Game   as Game
import qualified Chesssimple.Player as Player
import Chesssimple.Board (Position)
import Chesssimple.Color

import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Split (splitOn)

main :: IO ()
main =
  let player1 = Player.new "Lautaro"
      player2 = Player.new "Sabrina"
      game    = Game.new player1 player2
   in do
     putStrLn "Welcome to Simple Chess Game"
     performGameTurn game

performGameTurn :: Game.Game -> IO ()
performGameTurn game
  | Game.isCheckMate game = do
      putStrLn $ "Game finished! " ++ colorTurn game ++ " loses!"
  | otherwise            = do
      putStrLn $ Game.show game
      putStrLn $ "It's " ++ colorTurn game ++ " move" ++ showCheckStatus game ++  ". Commands are: exit, which, move"
      userInput <- getLine
      case parseCommand userInput of
        ("exit" ,         _) -> return ()
        ("which",     pos:_) -> do
          putStrLn $ showAvailableMovements game (parsePosition pos)
          performGameTurn game
        ("move" , src:dst:_) -> performMove game (parsePosition src) (parsePosition dst)
        _ -> do
          putStrLn $ "Bad command. Try again."
          performGameTurn game

parseCommand :: String -> (String, [String])
parseCommand userInput = let command:args = splitOn " " userInput
                          in (command, args)

parsePosition :: String -> Maybe Position
parsePosition pos = let a:b:_ = map digitToInt (take 2 pos)
                     in Just (a,b)

performMove :: Game.Game -> Maybe Position -> Maybe Position -> IO ()
performMove game Nothing dst = do
  putStrLn $ "Bad source position"
  performGameTurn game
performMove game src Nothing = do
  putStrLn $ "Bad destiny position"
  performGameTurn game
performMove game (Just src) (Just dst) =
  case (Game.tryMovement game src dst) of
    Just nextGame -> performGameTurn nextGame
    Nothing -> do
      putStrLn "Illegal movement. Try again."
      performGameTurn game

showAvailableMovements :: Game.Game -> Maybe Position -> String
showAvailableMovements game Nothing         = "No movements"
showAvailableMovements game (Just position) = let movs = Game.availableMovements game position
                                               in if null movs then "No movements"
                                                               else intercalate ", " $ map show movs

colorTurn :: Game.Game -> String
colorTurn game = case Game.turn game of
                   White -> "White"
                   Black -> "Black"

showCheckStatus :: Game.Game -> String
showCheckStatus game = if Game.isCheck game then " (and is in CHECK) "
                                            else ""
