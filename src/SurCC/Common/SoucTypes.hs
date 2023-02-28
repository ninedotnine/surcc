{-# LANGUAGE PatternSynonyms #-}

module SurCC.Common.SoucTypes (
    SoucType(..),
    pattern SoucType,
    SoucKind(..),
    recurse_kind,
    TypeVar(..),
    pattern SoucIO,
    pattern SoucBool,
    pattern SoucInteger,
    pattern SoucChar,
    pattern SoucString,
    pattern SoucFn,
    pattern SoucRoutn,
    pattern SoucMaybe,
    pattern SoucList,
    pattern SoucPair,
    pattern SoucEither,
    pattern SoucModuleType,
    ) where

import Data.Text (Text)
import TextShow (showt)


data SoucType = SoucTypeCon Text SoucKind [SoucType]
              | SoucTypeVar TypeVar
--               | SoucConstrainedType Constraint SoucType
              deriving (Eq,Show,Ord)


data SoucKind = KType
              | KFunc SoucKind SoucKind
    deriving (Eq, Ord, Show)


recurse_kind :: Int -> SoucKind
recurse_kind k = foldr ($) KType (replicate k (KFunc KType))

-- allowed type names are single chars like 'A'
-- or 'T' followed by an increasing number (T0, T1, ...)
data TypeVar = TypeVar (Either Word Char) SoucKind deriving (Eq, Ord)

instance Show TypeVar where
    show (TypeVar v _) = case v of
        Left i -> 'T' : show i
        Right c -> [c]


-- data Constraint = Instance Text SoucType deriving (Eq)


pattern SoucType :: Text -> SoucType
pattern SoucType name = SoucTypeCon name KType []

pattern SoucIO :: SoucType
pattern SoucIO = SoucType "IO"

pattern SoucBool :: SoucType
pattern SoucBool = SoucType "Bool"

pattern SoucInteger :: SoucType
pattern SoucInteger = SoucType "Integer"

pattern SoucChar :: SoucType
pattern SoucChar = SoucType "Char"

pattern SoucString :: SoucType
pattern SoucString = SoucType "String"

pattern SoucFn :: SoucType -> SoucType -> SoucType
pattern SoucFn t0 t1 =
    SoucTypeCon "Fn" (KFunc KType (KFunc KType KType)) [t0,t1]

pattern SoucRoutn :: SoucType -> SoucType
pattern SoucRoutn t = SoucTypeCon "Sub" (KFunc KType KType) [t]

pattern SoucMaybe :: SoucType -> SoucType
pattern SoucMaybe t = SoucTypeCon "Maybe" (KFunc KType KType) [t]

pattern SoucList :: SoucType -> SoucType
pattern SoucList t = SoucTypeCon "List" (KFunc KType KType) [t]

pattern SoucPair :: SoucType -> SoucType -> SoucType
pattern SoucPair t0 t1 =
    SoucTypeCon "Pair" (KFunc KType (KFunc KType KType)) [t0,t1]

pattern SoucEither :: SoucType -> SoucType-> SoucType
pattern SoucEither t0 t1 =
    SoucTypeCon "Either" (KFunc KType (KFunc KType KType)) [t0,t1]

-- FIXME modules could have a phantom type to distinguish them
pattern SoucModuleType :: SoucType
pattern SoucModuleType = SoucType "Module"
