{-# LANGUAGE TemplateHaskell #-}

module Text.HsDoc.TH
  ( DerivFunc,
    deriv,
    baseName,
    decToName,
    getRename,
    getRenamedLit,
    conPattern,
  )
where

import Control.Applicative
import Data.HashMap (Map, findWithDefault, fromList)
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Text.HsDoc.Types (RenameAnnon, RenameTab)

instance (Semigroup a) => Semigroup (Q a) where
  (<>) a b = liftA2 (<>) a b

instance (Monoid a) => Monoid (Q a) where
  mempty = return mempty

type DerivFunc = DecQ -> RenameTab -> DecsQ

getDataDec :: Name -> DecQ
getDataDec n = do
  inf <- reify n
  case inf of
    TyConI d -> return d
    _ -> fail "Only data is supported"

baseName :: Name -> Name
baseName = mkName . nameBase

decToName :: Dec -> Q Name
decToName (DataD _ name _ _ _ _) = return name
decToName _ = fail "Not supported type"

getRename :: Name -> RenameTab -> String
getRename n t =
  let bn = nameBase n
   in findWithDefault bn bn t

getRenamedLit :: Name -> RenameTab -> ExpQ
getRenamedLit n t = litE $ stringL $ getRename n t

conPattern :: Con -> PatQ
conPattern (RecC name fields) =
  recP name $
    fmap (\(n, _, _) -> fieldPat n $ varP $ baseName n) fields
conPattern _ = fail "Only record is supported"

deriv :: Name -> [DerivFunc] -> DecsQ
deriv n fs = do
  ren <- reifyAnnotations (AnnLookupName n) :: Q [RenameAnnon]
  let d = getDataDec n
      reMap = fromList $ mconcat ren
  mconcat $ map (\x -> x d reMap) fs
