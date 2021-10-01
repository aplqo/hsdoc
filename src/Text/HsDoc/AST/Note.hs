module Text.HsDoc.AST.Note where

import Data.Text (Text)
import {-# SOURCE #-} Text.HsDoc.AST (Inline)

data Place = Foot | Margin

data Note = Note
  { text :: Maybe Text,
    note :: [Inline],
    place :: Place
  }
