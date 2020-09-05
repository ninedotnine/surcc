{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.TypeChecker (
    type_check,
    ) where

import Control.Applicative
import Data.Either

import Prelude hiding (lookup)
import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context
import TypeChecker.Operators
import TypeChecker.Expressions

import Debug.Trace

type Checker a = Either TypeError a


-- FIXME this should fail sometimes lol
type_check :: Program -> Either TypeError CheckedProgram
type_check (Program name imports defns) = do
--     debug prog
    traceM ("top lvl conts: " ++ show (get_top_level_const_defns defns))
    global_bounds <- get_top_level_const_bounds_or_fails_end defns
    traceM ("defns: " ++ show defns)
    traceM ("global_bounds: " ++ show global_bounds)
    type_check_internal (Program name imports defns)
--     where debug p = traceM $
-- --             "BOUND!! " ++ show (get_globals p) ++ "\n tree was: " ++ show prog
--             "BOUND!! " ++ show (get_globals p) ++ "\n tree was: " ++ show prog

type_check_internal :: Program -> Either TypeError CheckedProgram
type_check_internal (Program name imports defns) = do
    globals <- get_globals imports defns
    Right $ CheckedProgram name imports defns

get_globals :: [Import] -> [Top_Level_Defn] -> Either TypeError Context
get_globals imports defns = do
    imps <- get_imports imports
    consts <- get_top_level_const_bounds_or_fails_end defns
    return $ Global $ imps ++ consts
--     walk_top_level_statements defns


-- getting imports can fail if (e. g.) a file cannot be found.
-- don't worry about it for now.
get_imports :: Imports -> Either TypeError [Bound]
get_imports imports = Right $ map make_import_bound (map from_import imports)
    where
        from_import :: Import -> String
        from_import (Import s) = s
        make_import_bound s = Bound (Identifier s) (TypeName "Module")


check_any_failed :: [Either TypeError Bound] -> Either TypeError [Bound]
check_any_failed list = let (ls, rs) = partitionEithers list in case ls of
    [] -> Right rs
    (x:_) -> Left x


get_top_level_const_bounds_or_fails_end :: [Top_Level_Defn] -> Either TypeError [Bound]
get_top_level_const_bounds_or_fails_end defns = let
    tldefs =  get_top_level_const_bounds_or_fails defns
    in traceM (show tldefs) >> check_any_failed tldefs

get_top_level_const_bounds_or_fails :: [Top_Level_Defn] -> [Either TypeError Bound]
get_top_level_const_bounds_or_fails defns = let
    tldefs = get_top_level_const_defns defns
    in map check_top_level_const_defns tldefs


empty_context :: Context
empty_context = Global []

check_top_level_const_defns :: Top_Level_Defn -> Either TypeError Bound
check_top_level_const_defns stmt = case stmt of
    Top_Level_Const_Defn i Nothing expr ->
        case check_astree empty_context expr inferred of
            Nothing -> Right (Bound i inferred)
            Just err -> Left err
            where inferred = infer empty_context expr
    Top_Level_Const_Defn i (Just t) expr -> if t == inferred
        then Right (Bound i t)
        else Left (TypeError t inferred)
            where inferred = infer empty_context expr
    _ -> error "FIXME haskell's type system can stop this"

get_top_level_const_defns :: [Top_Level_Defn] -> [Top_Level_Defn]
get_top_level_const_defns = filter is_top_level_const_defn where
    is_top_level_const_defn :: Top_Level_Defn -> Bool
    is_top_level_const_defn (Top_Level_Const_Defn _ _ _) = True
    is_top_level_const_defn _ = False

walk_top_level_statements :: [Top_Level_Defn] -> [Bound]
walk_top_level_statements defns = map unroll defns where
    unroll :: Top_Level_Defn -> Bound
    unroll defn = case defn of
        Top_Level_Const_Defn ident (Just t) _ -> Bound ident t
        Top_Level_Const_Defn ident Nothing  _ -> Bound ident "UnknownConst"
        FuncDefn ident _ (Just t) _ -> Bound ident ("a -> " <> t)
        FuncDefn ident _ Nothing  _ -> Bound ident "a -> b"
        ShortFuncDefn ident _ (Just t) _ -> Bound ident ("Fn a " <> t)
        ShortFuncDefn ident _ Nothing  _ -> Bound ident ("Fn a b")
        SubDefn ident _ (Just t) _ -> Bound ident ("What -> " <> t)
        SubDefn ident _ Nothing  _ -> Bound ident ("What -> IO")
        MainDefn _ (Just t) _ -> Bound "main" ("Args -> " <> t)
        MainDefn _ Nothing  _ -> Bound "main" ("Args -> IO")

