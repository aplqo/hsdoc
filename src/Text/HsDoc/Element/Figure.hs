module Text.HsDoc.Element.Figure where

import Data.Text (Text)
import qualified Text.HsDoc.AST as AST
import qualified Text.HsDoc.AST.Figure as AF
import Text.HsDoc.Element
  ( Config,
    ToInline (..),
    ToInlines (..),
  )

data Figure a = Figure
  { title :: Maybe Text,
    target :: Text,
    alt :: Maybe a,
    align :: AF.Align
  }

instance ToInlines d e => ToInline d (Figure e) where
  toInline c s f =
    ( AST.Meta,
      AST.Figure
        AF.Figure
          { title = title f,
            target = target f,
            alt = toInlines c s <$> alt f,
            align = align f
          }
    )

instance ToInlines d e => ToInlines d (Figure e)
