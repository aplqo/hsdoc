module Text.HsDoc.AST.OrderedList where

import Data.Default

data NumStyle
  = DefaultStyle
  | Example
  | Decimal
  | LowerRoman
  | UpperRoman
  | LowerAlpha
  | UpperAlpha
  deriving (Show)

data NumDelim
  = DefaultDelim
  | Period
  | OneParen
  | TwoParens
  deriving (Show)

data Attrib = Attrib
  { start :: Int,
    style :: NumStyle,
    delim :: NumDelim
  }
  deriving (Show)

instance Default Attrib where
  def =
    Attrib
      { start = 1,
        style = DefaultStyle,
        delim = DefaultDelim
      }
