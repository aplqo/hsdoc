module Text.HsDoc.AST.Figure where

import Data.Maybe (Maybe)
import Data.Text (Text)
import {-# SOURCE #-} Text.HsDoc.AST (Inline)

data Align = Left | Right | Default | Center

data Figure = Figure
  { path :: Text,
    alt :: Maybe [Inline],
    align :: Align
  }
