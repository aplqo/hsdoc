module Text.HsDoc.AST.Code where

import Data.Maybe (Maybe)
import Data.Text (Text)

data Block = Block
  { language :: Maybe Text,
    lineNum :: Bool,
    startNum :: Maybe Int,
    highlight :: [Int],
    code :: Text
  }
  deriving (Show)

data Inline = Inline
  { language :: Maybe Text,
    code :: Text
  }
  deriving (Show)
