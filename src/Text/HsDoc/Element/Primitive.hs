module Text.HsDoc.Element.Primitive where

import Data.Default
import qualified Data.Text as T
import qualified Text.HsDoc.AST as AST
import Text.HsDoc.Element

instance ToInline d () where
  toInline _ _ _ = (AST.Meta, AST.NullInline)

instance ToInlines d ()

instance ToBlock d () where
  toBlock _ _ _ = (AST.Meta, AST.NullBlock)

instance ToBlocks d ()

text :: T.Text -> [AST.Inline]
text t = map conv $ T.groupBy (\x y -> is_space x == is_space y) t
  where
    is_space x = x `elem` ['\t', '\n', ' ', '\r']
    is_newline x = x == '\r' || x == '\n'
    conv t
      | T.any is_newline t = (AST.Meta, AST.SoftBreak)
      | T.any is_space t = (AST.Meta, AST.Space)
      | otherwise = (AST.Meta, AST.Str t)

instance ToInlines d T.Text where
  toInlines _ _ = text

instance ToBlock d T.Text

instance ToBlocks d T.Text

instance (ToBlocks d b) => ToBlock d [b] where
  toBlock c d e = (AST.Meta, AST.OrderedList def $ map (toBlocks c d) e)

instance (ToBlocks d b) => ToBlocks d [b]
