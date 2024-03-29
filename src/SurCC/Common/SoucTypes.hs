{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SurCC.Common.SoucTypes (
    SoucType(..),
    pattern SoucType,
    TypeCon(..),
    TypeVar(..),
    Rigidity(..),
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

import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text


data SoucType = SoucTypeCon TypeCon [SoucType]
              | SoucTypeVar TypeVar [SoucType]
              | SoucForAll TypeVar Rigidity SoucType
--               | SoucConstrainedType Constraint SoucType
              deriving (Eq,Show,Ord)


-- FIXME TypeCon and TypeVar will need to know their kinds
newtype TypeCon = TypeCon Text deriving (Eq, Ord, IsString)

-- allowed type names are single chars like 'A'
-- or 'T' followed by an increasing number (T0, T1, ...)
newtype TypeVar = TypeVar (Either Word Char) deriving (Eq, Ord)


instance Show TypeCon where
    show (TypeCon txt) = Text.unpack txt


instance Show TypeVar where
    show (TypeVar v) = case v of
        Left i -> 'T' : show i
        Right c -> [c]


data Rigidity = Rigid | Wobbly
              deriving (Eq,Show,Ord)

-- data Constraint = Instance Text SoucType deriving (Eq)


pattern SoucType :: Text -> SoucType
pattern SoucType name = SoucTypeCon (TypeCon name) []

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
    SoucTypeCon "Fn" [t0,t1]

pattern SoucRoutn :: SoucType -> SoucType
pattern SoucRoutn t = SoucTypeCon "Sub" [t]

pattern SoucMaybe :: SoucType -> SoucType
pattern SoucMaybe t = SoucTypeCon "Maybe" [t]

pattern SoucList :: SoucType -> SoucType
pattern SoucList t = SoucTypeCon "List" [t]

pattern SoucPair :: SoucType -> SoucType -> SoucType
pattern SoucPair t0 t1 =
    SoucTypeCon "Pair" [t0,t1]

pattern SoucEither :: SoucType -> SoucType-> SoucType
pattern SoucEither t0 t1 =
    SoucTypeCon "Either" [t0,t1]

-- FIXME modules could have a phantom type to distinguish them
pattern SoucModuleType :: SoucType
pattern SoucModuleType = SoucType "Module"
