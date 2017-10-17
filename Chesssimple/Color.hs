module Chesssimple.Color (Color(Black, White), switch) where

data Color = White | Black deriving (Eq)

instance Show Color where
  show Black  = "b"
  show White  = "w"

switch :: Color -> Color
switch Black = White
switch White = Black
