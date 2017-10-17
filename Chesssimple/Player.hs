module Chesssimple.Player ( Player, new ) where

data Player = Player { name :: String
                     } deriving (Show)

new :: String -> Player
new name = Player { name=name }
