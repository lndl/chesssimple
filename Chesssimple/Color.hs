module Chesssimple.Color (Color(Black, White), switch) where

data Color = White | Black deriving (Eq)

instance Show Color where
  show Black  = "-"
  show White  = "+"

switch :: Color -> Color
switch Black = White
switch White = Black
