module Text.HsDoc.Element.Code where

import qualified Text.HsDoc.AST as AST
import Text.HsDoc.AST.Code (Block, Inline)
import Text.HsDoc.Element
  ( ToBlock (..),
    ToInline (..),
    ToInlines (..),
  )

instance ToInline Inline where
  toInline s c = (AST.Meta, AST.Code c)

instance ToInlines Inline where
  toInlines s c = [(AST.Meta, AST.Code c)]

instance ToBlock Block where
  toBlock s c = (AST.Meta, AST.CodeBlock c)