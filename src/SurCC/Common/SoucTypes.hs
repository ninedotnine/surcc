{-# LANGUAGE PatternSynonyms #-}

module SurCC.Common.SoucTypes (
    SoucType(..),
    pattern SoucType,
    SoucKind(..),
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


                -- SoucTypeConstructor invariant:
                -- the length of the list of types
                -- must be equal to the SoucKind
                -- FIXME find a better way to represent this
data SoucType = SoucTypeConstructor Text SoucKind [SoucType]
              | SoucTypeVar TypeVar
--               | SoucConstrainedType Constraint SoucType
              deriving (Eq,Show,Ord)


newtype SoucKind = SoucKind Word deriving (Eq, Ord, Show)


-- allowed type names are single chars like 'A'
-- or 'T' followed by an increasing number (T0, T1, ...)
data TypeVar = TypeVar (Either Word Char) SoucKind deriving (Eq, Ord, Show)



-- data Constraint = Instance Text SoucType deriving (Eq)


pattern SoucType :: Text -> SoucKind -> SoucType
pattern SoucType name k = SoucTypeConstructor name k []

pattern SoucIO :: SoucType
pattern SoucIO = SoucType "IO" (SoucKind 0)

pattern SoucBool :: SoucType
pattern SoucBool = SoucType "Bool" (SoucKind 0)

pattern SoucInteger :: SoucType
pattern SoucInteger = SoucType "Integer" (SoucKind 0)

pattern SoucChar :: SoucType
pattern SoucChar = SoucType "Char" (SoucKind 0)

pattern SoucString :: SoucType
pattern SoucString = SoucType "String" (SoucKind 0)

pattern SoucFn :: SoucType -> SoucType -> SoucType
pattern SoucFn t0 t1 = SoucTypeConstructor "Fn" (SoucKind 2) [t0,t1]

pattern SoucRoutn :: SoucType -> SoucType
pattern SoucRoutn t = SoucTypeConstructor "Sub" (SoucKind 1) [t]

pattern SoucMaybe :: SoucType -> SoucType
pattern SoucMaybe t = SoucTypeConstructor "Maybe" (SoucKind 1) [t]

pattern SoucList :: SoucType -> SoucType
pattern SoucList t = SoucTypeConstructor "List" (SoucKind 1) [t]

pattern SoucPair :: SoucType -> SoucType-> SoucType
pattern SoucPair t0 t1 = SoucTypeConstructor "Pair" (SoucKind 2) [t0,t1]

pattern SoucEither :: SoucType -> SoucType-> SoucType
pattern SoucEither t0 t1 = SoucTypeConstructor "Either" (SoucKind 2) [t0,t1]

pattern SoucModuleType :: SoucType
pattern SoucModuleType = SoucType "Module" (SoucKind 0)

