module Chesssimple.Player ( Player, newPlayer ) where

data Player = Player { name :: String
                     } deriving (Show)

newPlayer :: String -> Player
newPlayer name = Player { name=name }
