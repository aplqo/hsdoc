module Text.HsDoc.Element.Figure where

import Data.Text (Text)
import qualified Text.HsDoc.AST as AST
import qualified Text.HsDoc.AST.Figure as AF
import Text.HsDoc.Element (ToInline (..), ToInlines (..))

data Figure a = Figure
  { path :: Text,
    alt :: Maybe a,
    align :: AF.Align
  }

instance (ToInlines a) => ToInline (Figure a) where
  toInline s f =
    ( AST.Meta,
      AST.Figure
        AF.Figure
          { path = path f,
            alt = toInlines s <$> alt f,
            align = align f
          }
    )

instance (ToInlines a) => ToInlines (Figure a) where
  toInlines s f = [toInline s f]