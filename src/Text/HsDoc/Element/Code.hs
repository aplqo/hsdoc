module Text.HsDoc.Element.Code where

import qualified Text.HsDoc.AST as AST
import Text.HsDoc.AST.Code (Block, Inline)
import Text.HsDoc.Element
  ( ToBlock (..),
    ToInline (..),
    ToInlines (..),
  )

instance ToInline d Inline where
  toInline _ _ c = (AST.Meta, AST.Code c)

instance ToInlines d Inline where
  toInlines _ _ c = [(AST.Meta, AST.Code c)]

instance ToBlock d Block where
  toBlock _ _ c = (AST.Meta, AST.CodeBlock c)
