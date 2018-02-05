{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import qualified Chesssimple.Game   as Game
import qualified Chesssimple.Player as Player
import qualified Chesssimple.Screen as Screen
import qualified Chesssimple.GameAI as GameAI
import Chesssimple.Board (Position)
import Chesssimple.Color

import Data.Char (digitToInt)
import Data.List (intercalate, elemIndex)
import Data.List.Split (splitOn)
import System.Console.CmdArgs

--- Command arguments definitions

data Args = HvH { p1name :: String, p1color :: String, p2name :: String      }
          | HvC { hname :: String,  hcolor :: String,  cstrength :: Integer }
          | CvC { p1strength :: Integer, p2strength :: Integer }
            deriving (Show, Data, Typeable)

humanvshuman = HvH {
  p1name  = "Player 1" &= help "The player 1's name",
  p1color = "white"    &= help "The player 1's color",
  p2name  = "Player 2" &= help "The player 2's name"
}

humanvscomputer = HvC {
  hname      = "Player 1" &= help "The human player's name",
  hcolor     = "white"    &= help "The human player's color",
  cstrength  = 3          &= help "The computer player's strength"
}

computervscomputer = CvC {
  p1strength  = 3       &= help "The player 1's strength",
  p2strength  = 3       &= help "The player 2's strength"
}

parseColor :: String -> Color
parseColor "black" = Black
parseColor "white" = White

playersFromArgs :: Args -> (Player.Player, Player.Player)
playersFromArgs HvH{p1name=p1n, p1color=p1c, p2name=p2n} = (Player.mkHumanPlayer p1n (parseColor p1c), Player.mkHumanPlayer    p2n (switch $ parseColor p1c))
playersFromArgs HvC{hname=hn, hcolor=hc, cstrength=cs}   = (Player.mkHumanPlayer hn (parseColor hc),   Player.mkComputerPlayer cs  (switch $ parseColor hc))
playersFromArgs CvC{p1strength=p1s, p2strength=p2s}      = (Player.mkComputerPlayer p1s White, Player.mkComputerPlayer p2s Black)

--- Start of the program

main :: IO ()
main = do
  arguments <- cmdArgs (modes [humanvshuman, humanvscomputer &= auto, computervscomputer] &= program "Chesssimple" &= help "Chess game & engine")
  Screen.reset
  Screen.printWithColor "Welcome to Simple Chess Game" "white"
  let (player1, player2) = playersFromArgs arguments
      game               = Game.new player1 player2
   in performGameTurn game

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
parsePosition pos = if length pos == 2
                    then let a:b:_ = pos in parsePosition' a b
                    else Nothing
  where
    parsePosition' column row = do
      c <- (+1) <$> elemIndex column ['a'..'h']
      r <- (+9) <$> negate <$> safeDigitToInt row
      return (r, c)
    safeDigitToInt r = if elem r ['1'..'8']
                       then Just $ digitToInt r
                       else Nothing
  

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
showAvailableMovements game (Just position) =
  let movs = Game.availableMovements game position
   in if null movs then "No movements"
      else intercalate ", " $ map showPosition movs
  where showPosition (r, c) = (['a'..'h'] !! (c - 1)):(show (9 - r))

colorTurn :: Game.Game -> String
colorTurn game = case Game.turn game of
                   White -> "White"
                   Black -> "Black"

showCheckStatus :: Game.Game -> String
showCheckStatus game = if Game.isCheck game then " (and is in CHECK) "
                                            else ""
