{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module SurCC.TypeChecker.Typedefs (
    build_typedefs,
) where

import Control.Arrow (second)
import Control.Monad.Error.Class
import Data.Functor
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict (HashMap)

import SurCC.Builtins (builtin_identifiers, builtin_types)
import SurCC.Common
import SurCC.Common.Hashable
import SurCC.Common.SoucTypes
import SurCC.TypeChecker.Context
import SurCC.TypeChecker.Expressions (assert_equals)

type Mapping = HashMap Identifier SoucType


build_typedefs :: [TypeDef] -> Either TypeError (GlobalScope,[Bound])
build_typedefs defs = do
    build_typedefs' (TypeSet builtin_types, builtin_identifiers) [] defs


build_typedefs' :: (TypeSet, Mapping) -> [Bound] -> [TypeDef]
                   -> Either TypeError (GlobalScope,[Bound])
build_typedefs' (types, consts) bounds = \case
    (d:defs) -> do
        (t, refut, terms) <- build_typedef d
        updated_types <- insert_type types t refut
        updated_consts <- insert_consts consts terms t
        let abounds = (terms <&> (\term -> Bound term t)) <> bounds
        build_typedefs' (updated_types, updated_consts) abounds defs
    [] -> pure $ (GlobalScope types consts, bounds)


-- FIXME can this function fail? does it need Either TypeError?
build_typedef :: TypeDef
                 -> Either TypeError (SoucType, Refutable, [Identifier])
build_typedef = pure <$> \case
    EmptyType t -> (t, Refutable True, [])
    UnitType t term -> (t, Refutable False, [term])
    SynonymType t0 t1 -> undefined t0 t1 -- FIXME
    WrapperType t0 constructor t1 ->
        (undefined t0 t1, Refutable False, [constructor]) -- FIXME
    EnumType t constructors ->
        -- FIXME fail if two constructors are equal
        (t, Refutable True, constructors) -- FIXME
    StructType t fns ->
        -- FIXME we're gonna need the bounds i think
        (t, Refutable False, fns <&> get_id)
        where get_id (Bound i _) = i
    GADType t -> undefined t -- FIXME


insert_consts :: Mapping -> [Identifier] -> SoucType
                 -> Either TypeError Mapping
insert_consts m i t = case i of
    (con:cons) -> do
        new_map <- insert_const m con t
        insert_consts new_map cons t
    [] -> pure m


insert_const :: Mapping -> Identifier -> SoucType
                -> Either TypeError Mapping
insert_const m i t = insert m i t `or_left` MultipleDeclarations i


insert_type :: TypeSet -> SoucType -> Refutable
               -> Either TypeError TypeSet
insert_type (TypeSet m) t r = (insert m t r <&> TypeSet)
                              `or_left` MultipleTypeDeclarations t


insert :: (Hashable k) => HashMap k v -> k -> v -> Maybe (HashMap k v)
insert m con t = if Map.member con m
    then Nothing
    else Just (Map.insert con t m)
