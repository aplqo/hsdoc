module Text.HsDoc.AST.OrderedList where

data NumStyle = 
  DefaultStyle
  | Example
  | Decimal
  | LowerRoman
  | UpperRoman
  | LowerAlpha
  | UpperAlpha

data NumDelim = 
  DefaultDelim
  | Period
  | OneParen
  | TwoParens

data Attrib = Attrib {
  start :: Int,
  style :: NumStyle,
  delim :: NumDelim }
