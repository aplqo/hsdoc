module Text.HsDoc.AST where

import Data.Text (Text)
import qualified Text.HsDoc.AST.Admonition as Admonition
import qualified Text.HsDoc.AST.Code as Code
import qualified Text.HsDoc.AST.Critic as Critic
import qualified Text.HsDoc.AST.Figure as Figure
import qualified Text.HsDoc.AST.Note as Note
import qualified Text.HsDoc.AST.OrderedList as OrderedList

data Meta = Meta deriving (Show)

type Inline = (Meta, InlineElem)

data InlineElem
  = Str Text
  | Emph [Inline]
  | Underline [Inline]
  | Strong [Inline]
  | Strikeout [Inline]
  | Superscript [Inline]
  | Subscript [Inline]
  | Quoted [Inline]
  | Space
  | SoftBreak
  | LineBreak
  | Math Text
  | KeyStoke [Text]
  | -- | Raw inline. A list of lines, linebreaks should be included. Writer will adjust indention.
    RawInline [Text]
  | Code Code.Inline
  | Critic Critic.Inline
  | Link
      { title :: Maybe Text,
        alt :: Maybe [Inline],
        target :: Text
      }
  | Figure Figure.Figure
  | Note Note.Note
  | NullInline
  deriving (Show)

type Block = (Meta, BlockElem)

data BlockElem
  = Plain [Inline]
  | Para [Inline]
  | CodeBlock Code.Block
  | BlockQuote [Block]
  | Admonition Admonition.Admonition
  | BulletList [[Block]]
  | OrderedList OrderedList.Attrib [[Block]]
  | DefinitionList [([Inline], [[Block]])]
  | HorzontalRule
  | MathBlock
      { wrap :: Bool,
        body :: [Text]
      }
  | TaskList [(Bool, [Block])]
  | CriticBlock Critic.Block
  | -- | Raw block. A list of lines, but no linebreak is added. Writer will adjust indention.
    RawBlock [Text]
  | NullBlock
  deriving (Show)

data Document = Document
  { title :: Text,
    file :: Maybe Text,
    meta :: Meta,
    body :: [Block],
    subtree :: [Document]
  }
  deriving (Show)
