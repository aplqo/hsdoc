module Text.HsDoc.AST.OrderedList where

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
