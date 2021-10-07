{-# LANGUAGE TemplateHaskell #-}

module Text.HsDoc.Element.PropertyList (derivToBlock) where

import qualified Data.HashMap as M
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import qualified Text.HsDoc.AST as AST
import Text.HsDoc.Element
import Text.HsDoc.Element.Primitive
import Text.HsDoc.TH
import Text.HsDoc.Types (RenameTab)

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

derivToBlock :: DerivFunc
derivToBlock d m = do
  cons <- d
  name <- decToName cons
  [d|
    instance ToBlock d $(conT name) where
      toBlock = $(deriv cons)

    instance ToBlocks d $(conT name)
    |]
  where
    deriv :: Dec -> ExpQ
    deriv (DataD _ _ _ _ [c] _) = toBlockFromCon c m
    deriv _ = fail "Only ADT with only one record constructor is supported"
