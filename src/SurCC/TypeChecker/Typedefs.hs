{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module SurCC.TypeChecker.Typedefs (
    build_typedefs,
) where

import Control.Arrow (second)
import Control.Monad.Error.Class
import Data.Functor
import Data.Function
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict (HashMap)
import Data.Maybe (maybeToList)

import SurCC.Builtins (builtin_identifiers, builtin_types)
import SurCC.Common
import SurCC.Common.Hashable
import SurCC.Common.SoucTypes
import SurCC.TypeChecker.Context
import SurCC.TypeChecker.Expressions (assert_equals)


build_typedefs :: [TypeDef] -> Either TypeError (GlobalScope,[Bound])
build_typedefs defs = do
    build_typedefs' (TypeConSet builtin_types,
                     ImmutMapping builtin_identifiers)
                    [] defs


build_typedefs' :: (TypeConSet, ImmutMapping) -> [Bound] -> [TypeDef]
                   -> Either TypeError (GlobalScope,[Bound])
build_typedefs' (types, consts) bounds = \case
    (d:defs) -> do
        let (t, refut, terms) = build_typedef d
        updated_types <- insert_type types t refut
        updated_consts <- insert_consts consts terms
        let abounds = terms <> bounds
        build_typedefs' (updated_types, updated_consts) abounds defs
    [] -> pure $ (GlobalScope types consts, bounds)


build_typedef :: TypeDef -> (TypeCon, Refutable, [Bound])
build_typedef = \case
    EmptyType t -> (t, Refutable True, [])
    UnitType t term -> (t, Refutable False, [Bound term (SoucTypeCon t [])])
    SynonymType t0 t1 -> undefined t0 t1 -- FIXME
    IsomorphismType t constructor constructor_t b ->
        (t, Refutable False, (Bound constructor constructor_t):maybeToList b)
    EnumType t constructors -> (t,
        Refutable True, (constructors <&> (\con -> Bound con (SoucTypeCon t []))))
    StructType t fns -> (t, Refutable False, fns)
    GADType t -> undefined t -- FIXME


insert_consts :: ImmutMapping -> [Bound] -> Either TypeError ImmutMapping
insert_consts m = \case
    ((Bound i t):cons) -> do
        new_map <- insert_const m i t
        insert_consts new_map cons
    [] -> pure m


insert_const :: ImmutMapping -> Identifier -> SoucType
                -> Either TypeError ImmutMapping
insert_const (ImmutMapping m) i t =
    (ImmutMapping <$> insert m i t) `or_left` MultipleDeclarations i


insert_type :: TypeConSet -> TypeCon -> Refutable
               -> Either TypeError TypeConSet
insert_type (TypeConSet m) t r = (insert m t r <&> TypeConSet)
                              `or_left` MultipleTypeConDecls t


insert :: (Hashable k) => HashMap k v -> k -> v -> Maybe (HashMap k v)
insert m con t = if Map.member con m
    then Nothing
    else Just (Map.insert con t m)
