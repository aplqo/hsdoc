{-# LANGUAGE TemplateHaskell #-}

module Text.HsDoc.Element.PropertyList where

import qualified Data.HashMap as M
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import qualified Text.HsDoc.AST as AST
import Text.HsDoc.Element
import Text.HsDoc.Element.Primitive
import Text.HsDoc.TH
import Text.HsDoc.Types (RenameTab)

conPattern :: Con -> PatQ
conPattern (RecC name fields) =
  recP name $
    fmap (\(n, _, _) -> fieldPat n $ varP $ baseName n) fields
conPattern _ = fail "Only record is supported"

toBlockFromCon :: Con -> RenameTab -> ExpQ
toBlockFromCon con@(RecC name fields) trans =
  [|
    \c s $(conPattern con) ->
      ( AST.Meta,
        AST.DefinitionList
          $( listE $
               map
                 ( \(n, _, _) ->
                     [|
                       ( text $(getRenamedLit n trans),
                         [toBlocks c s $(varE $ baseName n)]
                       )
                       |]
                 )
                 fields
           )
      )
    |]
toBlockFromCon _ _ = fail "Constructor must be record"

getName :: Dec -> Q Name
getName (DataD _ name _ _ _ _) = return name
getName _ = fail "Not supported type"

derivToBlock :: DerivFunc
derivToBlock d m = do
  cons <- d
  name <- getName cons
  [d|
    instance ToBlock d $(conT name) where
      toBlock = $(deriv cons)

    instance ToBlocks d $(conT name)
    |]
  where
    deriv :: Dec -> ExpQ
    deriv (DataD _ _ _ _ [c] _) = toBlockFromCon c m
    deriv _ = fail "Only ADT with only one record constructor is supported"
