module Text.HsDoc.Element where

import Text.HsDoc.AST
  ( Block,
    BlockElem (NullBlock),
    Document,
    Inline,
    InlineElem (NullInline),
    Meta (Meta),
  )

data State = State

class ToDocument a where
  toDocument :: State -> a -> Document

class ToInline a where
  toInline :: State -> a -> Inline

class ToInlines a where
  toInlines :: State -> a -> [Inline]

class ToBlock a where
  toBlock :: State -> a -> Block

instance ToInline () where
  toInline _ _ = (Meta, NullInline)

instance ToInlines () where
  toInlines _ _ = [(Meta, NullInline)]

instance ToBlock () where
  toBlock _ _ = (Meta, NullBlock)