module Text.HsDoc.AST.Code where

import Data.Maybe (Maybe)
import Data.Text (Text)

data Block = Block
  { language :: Maybe Text,
    lineNum :: Bool,
    highlight :: [Int],
    code :: Text
  }

data Inline = Inline
  { language :: Maybe Text,
    code :: Text
  }
