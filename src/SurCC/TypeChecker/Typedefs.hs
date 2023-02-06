{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SurCC.TypeChecker.Typedefs (
    build_typedefs,
    TypeSet,
) where

import Data.Functor
import Data.Map.Strict qualified as Map
import Data.Map.Strict (Map)

import SurCC.Common
import SurCC.TypeChecker.Context

import Debug.Trace

type Mapping = Map Identifier SoucType

newtype TypeSet = TypeSet (Map SoucType Refutable) deriving (Show)

-- whether a pattern can be used in a match that might fail
newtype Refutable = Refutable Bool deriving (Eq, Ord, Show)


build_typedefs :: [TypeDef] -> ExportList
                  -> Either TypeError (TypeSet, LocalScope)
build_typedefs defs exports = do
    traceM $ " defs : " <> show defs
    build_typedefs' (default_types, Map.empty, exports) defs <&> wrap
        where
            -- FIXME remove types and constructors from ctx
            wrap (ts,m,exps) = (ts, GlobalScope m exps)


build_typedefs' :: (TypeSet, Mapping, ExportList) -> [TypeDef]
                   -> Either TypeError (TypeSet, Mapping, ExportList)
build_typedefs' (types, consts, exps) = \case
    (d:defs) -> do
        (t, refut, terms) <- build_typedef d
        updated_types <- insert_type types t refut
        updated_consts <-
             -- also FIXME get rid of Constant, use Identifier
            insert_consts consts terms t
        new_exps <- remove_exports exps terms t
        build_typedefs' (updated_types, updated_consts, new_exps) defs
    [] -> pure (types, consts, exps)


build_typedef :: TypeDef
                 -> Either TypeError (SoucType, Refutable, [Identifier])
build_typedef = \case
    EmptyType t -> pure (t, Refutable True, [])
    UnitType t term -> pure (t, Refutable False, [term])
    SynonymType t0 t1 -> undefined t0 t1 -- FIXME
    WrapperType t0 constructor t1 ->
        pure (undefined t0 t1, Refutable False, [constructor]) -- FIXME
    EnumType t constructors ->
        -- FIXME fail if two constructors are equal
        pure (t, Refutable True, constructors) -- FIXME
    StructType t -> pure (t, Refutable False, []) -- FIXME
    GADType t -> undefined t -- FIXME


insert_consts :: Mapping -> [Identifier] -> SoucType
                 -> Either TypeError Mapping
insert_consts m i t = case i of
    (con:cons) -> do
        new_map <- insert_const m con t
        insert_consts new_map cons t
    [] -> pure Map.empty


insert_const :: Mapping -> Identifier -> SoucType
                -> Either TypeError Mapping
insert_const m i t = case insert m i t of
    Just new_map -> Right new_map
    Nothing  -> Left (MultipleDeclarations i)


insert_type :: TypeSet -> SoucType -> Refutable
               -> Either TypeError TypeSet
insert_type (TypeSet m) t r = case insert m t r of
    Just new_map -> Right (TypeSet new_map)
    Nothing  -> Left (MultipleTypeDeclarations t)


insert :: Ord k => Map k v -> k -> v -> Maybe (Map k v)
insert m con t = case Map.lookup con m of
    Just _ -> Nothing
    Nothing -> Just (Map.insert con t m)


remove_exports :: ExportList -> [Identifier]
                  -> SoucType -> Either TypeError ExportList
remove_exports = undefined

remove_export :: ExportList -> Identifier -> SoucType
                 -> Either TypeError ExportList
remove_export = undefined


default_types = TypeSet $ Map.fromList [
    (SoucBool, Refutable True),
    (SoucChar, Refutable True),
    (SoucInteger, Refutable True)
    ]
--     SoucString,


