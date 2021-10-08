{-# LANGUAGE TemplateHaskell #-}

module Text.HsDoc.Element.TOCTree where

import qualified Data.HashMap as M
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Text.HsDoc.AST
import Text.HsDoc.Element
import Text.HsDoc.TH
import Text.HsDoc.Types (RenameTab)

defField :: String -> ExpQ -> FieldExpQ
defField n = fieldExp (mkName ("Text.HsDoc.AST." ++ n))

toDocumentFromCon :: Con -> RenameTab -> ExpQ
toDocumentFromCon con@(RecC name field) trans =
  [|
    \t c s $(conPattern con) ->
      $( recConE
           (mkName "Text.HsDoc.AST.Document")
           [ defField "title" [|t|],
             defField "meta" [|Meta|],
             defField "file" [|Just t|],
             defField "body" [|[]|],
             defField "subtree" $
               listE $
                 map
                   ( \(n, _, _) ->
                       [|toDocument $(getRenamedLit n trans) c s $(varE $ baseName n)|]
                   )
                   field
           ]
       )
    |]
toDocumentFromCon _ _ = fail "Only record is supported"

derivToDocument :: DerivFunc
derivToDocument d m = do
  cons <- d
  name <- decToName cons
  [d|
    instance ToDocument d $(conT name) where
      toDocument = $(deriv cons)
    |]
  where
    deriv :: Dec -> ExpQ
    deriv (DataD _ _ _ _ [c] _) = toDocumentFromCon c m
    deriv _ = fail "Only ADT with one constructor is supported"
