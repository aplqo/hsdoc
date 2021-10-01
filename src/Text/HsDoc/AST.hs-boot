module Text.HsDoc.AST where

data Meta

data InlineElem

type Inline = (Meta, InlineElem)

data BlockElem

type Block = (Meta, BlockElem)

data Document