{-# LANGUAGE TemplateHaskell #-}

module Text.HsDoc.TH (DerivFunc, deriv, baseName) where

import Control.Applicative
import Data.HashMap (Map, fromList)
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

deriv :: Name -> [DerivFunc] -> DecsQ
deriv n fs = do
  ren <- reifyAnnotations (AnnLookupName n) :: Q [RenameAnnon]
  let d = getDataDec n
      reMap = fromList $ mconcat ren
  mconcat $ map (\x -> x d reMap) fs
