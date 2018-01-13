module Main where

import qualified Chesssimple.Game   as Game
import qualified Chesssimple.Player as Player
import qualified Chesssimple.Screen as Screen
import qualified Chesssimple.GameAI as GameAI
import Chesssimple.Board (Position)
import Chesssimple.Color

import Data.Char (digitToInt)
import Data.List (intercalate)
import Data.List.Split (splitOn)

main :: IO ()
main =
  let player1 = Player.mkHumanPlayer "Lautaro" White
      player2 = Player.mkComputerPlayer 2 Black
      game    = Game.new player1 player2
   in do
     Screen.reset
     Screen.printWithColor "Welcome to Simple Chess Game" "white"
     performGameTurn game

performGameTurn :: Game.Game -> IO ()
performGameTurn game = do
  printGameLayout game
  case Game.isCheckMate game of
    True  -> Screen.printWithColor ("Game finished! " ++ colorTurn game ++ " loses!") "red"
    False -> performGamePlay game $ Game.whoPlaysNow game

printGameLayout :: Game.Game -> IO ()
printGameLayout game = do
  Screen.setCursor 2 0
  Screen.clearUntilEnd
  putStrLn $ Game.show game
  Screen.printWithColor (colorTurn game ++ " moves..." ++ showCheckStatus game) "white"

performGamePlay :: Game.Game -> Player.Player -> IO ()
performGamePlay game (Player.ComputerPlayer strength _) =
  let updatedGame = GameAI.performMovement game strength
   in performGameTurn updatedGame
performGamePlay game (Player.HumanPlayer name _)    = do
  putStrLn "Commands are: exit, which, move, undo"
  userInput <- getLine
  case parseCommand userInput of
    ("exit" ,         _) -> return ()
    ("which",     pos:_) -> do
      Screen.printWithColor ("Available movements are: " ++ (showAvailableMovements game (parsePosition pos))) "white"
      Screen.pause
      performGameTurn game
    ("move" , src:dst:_) -> do
      performMoveAction game (parsePosition src) (parsePosition dst)
    ("undo" ,         _) -> performUndoAction game
    _ -> do
      Screen.printError "Bad command. Try again."
      performGameTurn game


performUndoAction :: Game.Game -> IO ()
performUndoAction game =
  if Game.isInitial game
  then do
    Screen.printError "Can't undo game!"
    performGameTurn game
  else do
    performGameTurn $ Game.undo game

parseCommand :: String -> (String, [String])
parseCommand userInput = let command:args = splitOn " " userInput
                          in (command, args)

parsePosition :: String -> Maybe Position
parsePosition pos = let a:b:_ = map digitToInt (take 2 pos)
                     in Just (a,b)

performMoveAction :: Game.Game -> Maybe Position -> Maybe Position -> IO ()
performMoveAction game Nothing dst = do
  Screen.printError "Bad source position"
  performGameTurn game
performMoveAction game src Nothing = do
  Screen.printError "Bad destiny position"
  performGameTurn game
performMoveAction game (Just src) (Just dst) =
  case (Game.tryMovement game src dst) of
    Just nextGame -> performGameTurn nextGame
    Nothing -> do
      Screen.printError "Illegal movement. Try again."
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
