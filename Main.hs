module Main where

import Chesssimple.Game
import Chesssimple.Player

main :: IO ()
main = let player1 = newPlayer "Lautaro"
           player2 = newPlayer "Sabrina"
        in putStrLn $ show $ newGame player1 player2
