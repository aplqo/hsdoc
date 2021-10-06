module Text.HsDoc.TH where

import Control.Applicative
import Language.Haskell.TH

instance (Semigroup a) => Semigroup (Q a) where
  (<>) a b = liftA2 (<>) a b

instance (Monoid a) => Monoid (Q a) where
  mempty = return mempty
