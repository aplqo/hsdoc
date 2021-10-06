module Text.HsDoc.Element where

import Text.HsDoc.AST
  ( Block,
    BlockElem (Plain),
    Document,
    Inline,
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
  default toInlines :: ToInline d e => Config d -> State -> e -> [Inline]
  toInlines c d e = [toInline c d e]

class ToBlock d e where
  toBlock :: Config d -> State -> e -> Block
  default toBlock :: ToInlines d e => Config d -> State -> e -> Block
  toBlock c d e = (Meta, Plain $ toInlines c d e)

class ToBlocks d e where
  toBlocks :: Config d -> State -> e -> [Block]
  default toBlocks :: ToBlock d e => Config d -> State -> e -> [Block]
  toBlocks c s e = [toBlock c s e]
