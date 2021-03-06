{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Chesssimple.Player ( Player(ComputerPlayer, HumanPlayer), mkHumanPlayer, mkComputerPlayer, name, strength, color ) where

import Chesssimple.Color
import Control.DeepSeq
import GHC.Generics (Generic)

data Player =
  HumanPlayer    { name :: String, color :: Color } |
    ComputerPlayer { strength :: Integer, color :: Color } deriving (Show, Generic, NFData)

mkHumanPlayer :: String -> Color -> Player
mkHumanPlayer name color = HumanPlayer { name=name, color=color }

mkComputerPlayer :: Integer -> Color -> Player
mkComputerPlayer strength color = ComputerPlayer { strength=strength, color=color }
