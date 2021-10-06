{-# LANGUAGE TemplateHaskell #-}

module Text.HsDoc.Record
  ( record,
    recordDeriv,
    bindVal,
    Value (E, N),
    (.->),
    (.=),
    (.=>),
  )
where

import Data.HashMap (Map, fromList)
import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax (sequenceQ)
import Text.HsDoc.TH
import Text.HsDoc.Types (RenameTab)

type AnonRec = (TypeQ, ExpQ, DecsQ)

data Value = E TypeQ ExpQ | N AnonRec

type NamedVal = (Maybe String, Value)

type Field = (String, NamedVal)

(.->) :: String -> Value -> NamedVal
n .-> v = (Just n, v)

infix 8 .=

(.=) :: String -> NamedVal -> Field
(.=) = (,)

(.=>) :: String -> Value -> Field
n .=> v = (n, (Nothing, v))

record :: String -> [Field] -> AnonRec
record name = recordDeriv name []

type FieldInfo = ([VarBangTypeQ], [(String, String)], [FieldExpQ], DecsQ)

recordDeriv :: String -> [DerivFunc] -> [Field] -> AnonRec
recordDeriv name deriv fields =
  let (f, ann, v, ds) = foldr collect ([], [], [], mempty) fields
      nm = mkName name
      annMap = fromList ann
      def = dataD (cxt []) nm [] Nothing [recC (mkName name) f] []
      drv = mconcat $ map (\f -> f def annMap) deriv
   in ( conT nm,
        recConE nm v,
        (do p <- def; return [p]) <> drv <> ds
      )
  where
    defaultBang = bang noSourceUnpackedness noSourceStrictness
    field name typ = varBangType name (bangType defaultBang typ)
    fieldName f n ns =
      case n of
        Just t -> (f, t) : ns
        Nothing -> ns

    collect :: Field -> FieldInfo -> FieldInfo
    collect (f, (n, vx)) (def, ann, val, decs) =
      let nm = mkName f
          (t, v, dec) = case vx of
            E t v -> (t, v, decs)
            N (t, v, ds) -> (t, v, ds <> decs)
       in ( field nm t : def,
            fieldName f n ann,
            fieldExp nm v : val,
            dec
          )

bindVal :: String -> AnonRec -> DecsQ
bindVal n (_, e, d) = [d|$(varP $ mkName n) = $(e)|] <> d
