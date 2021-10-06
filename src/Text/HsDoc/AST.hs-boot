module Text.HsDoc.AST where

data Meta

instance Show Meta

data InlineElem

instance Show InlineElem

type Inline = (Meta, InlineElem)

data BlockElem

instance Show BlockElem

type Block = (Meta, BlockElem)

data Document

instance Show Document
