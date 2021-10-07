module Text.HsDoc.AST.Figure where

import Data.Maybe (Maybe)
import Data.Text (Text)
import {-# SOURCE #-} Text.HsDoc.AST (Inline)

data Align = Left | Right | Default | Center deriving (Show)

data Figure = Figure
  { title :: Maybe Text,
    target :: Text,
    alt :: Maybe [Inline],
    align :: Align
  }
  deriving (Show)
