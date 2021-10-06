module Text.HsDoc.AST.Note where

import Data.Text (Text)
import {-# SOURCE #-} Text.HsDoc.AST (Inline)

data Place = Foot | Margin deriving (Show)

data Note = Note
  { text :: Maybe Text,
    note :: [Inline],
    place :: Place
  }
  deriving (Show)
