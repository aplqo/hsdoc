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

data family Config s

class ToDocument d e where
  toDocument :: Config d -> State -> e -> Document

class ToInline d e where
  toInline :: Config d -> State -> e -> Inline

class ToInlines d e where
  toInlines :: Config d -> State -> e -> [Inline]

class ToBlock d e where
  toBlock :: Config d -> State -> e -> Block

instance ToInline d () where
  toInline _ _ _ = (Meta, NullInline)

instance ToInlines d () where
  toInlines _ _ _ = [(Meta, NullInline)]

instance ToBlock d () where
  toBlock _ _ _ = (Meta, NullBlock)
