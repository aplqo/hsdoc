module Text.HsDoc.Element.Code (Block, Inline) where

import qualified Text.HsDoc.AST as AST
import Text.HsDoc.AST.Code (Block, Inline)
import Text.HsDoc.Element
  ( ToBlock (..),
    ToBlocks (..),
    ToInline (..),
    ToInlines (..),
  )

instance ToInline d Inline where
  toInline _ _ c = (AST.Meta, AST.Code c)

instance ToInlines d Inline

instance ToBlock d Block where
  toBlock _ _ c = (AST.Meta, AST.CodeBlock c)

instance ToBlocks d Block
