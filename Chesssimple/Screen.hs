module Chesssimple.Screen (
  reset,
  pause,
  clearUntilEnd,
  setCursor,
  printWithColor,
  printError
) where

import Control.Concurrent (threadDelay)
import System.Console.ANSI
import System.IO (hFlush, stdout)

reset :: IO ()
reset = setSGR [Reset] >> clearScreen >> setCursorPosition 0 0

pause :: IO ()
pause = do
  hFlush stdout
  threadDelay 2000000

clearUntilEnd :: IO ()
clearUntilEnd = clearFromCursorToScreenEnd

setCursor :: Int ->Int -> IO ()
setCursor x y = setCursorPosition x y

printWithColor :: String -> String -> IO ()
printWithColor text color = do
  setSGR [SetColor Foreground Vivid (parseColor color)]
  putStrLn text
  setSGR [SetColor Foreground Dull White]

printError :: String -> IO ()
printError text = do
  printWithColor text "red"
  pause

----------------------------
-- Private
----------------------------

parseColor :: String -> Color
parseColor "red"    = Red
parseColor "green"  = Green
parseColor "blue"   = Blue
parseColor "yellow" = Yellow
parseColor "white"  = White
