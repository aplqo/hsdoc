module Text.HsDoc.AST.Note where

import Data.Text (Text)
import {-# SOURCE #-} Text.HsDoc.AST (Block, Inline)

data Place = Foot | Margin deriving (Show)

data Note = Note
  { text :: Maybe [Inline],
    note :: [Block],
    place :: Place
  }
  deriving (Show)
