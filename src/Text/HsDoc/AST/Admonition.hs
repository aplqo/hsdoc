module Text.HsDoc.AST.Admonition where

import Data.Text (Text)
import {-# SOURCE #-} Text.HsDoc.AST (Block)

data Display = NotFoldable | Folded | Unfolded deriving (Show)

type Type = Text

data Admonition = Admonition
  { title :: Maybe Text,
    kind :: Type,
    display :: Display,
    body :: [Block]
  }
  deriving (Show)
