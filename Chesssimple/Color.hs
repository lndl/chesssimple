{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Chesssimple.Color (Color(Black, White), switch) where

import Control.DeepSeq
import GHC.Generics (Generic)

data Color = White | Black deriving (Eq, Generic, NFData)

instance Show Color where
  show Black  = "-"
  show White  = "+"

switch :: Color -> Color
switch Black = White
switch White = Black
