module Text.HsDoc.AST.Critic where

import {-# SOURCE #-} qualified Text.HsDoc.AST as AST

data Base a
  = Insert [a]
  | Delete [a]
  | Highlight [a]
  | Comment [a]
  | Substitution
      { from :: [a],
        to :: [a]
      }
  deriving (Show)

type Block = Base AST.Block

type Inline = Base AST.Inline
