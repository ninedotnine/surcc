{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.TypeChecker (
    type_check,
    ) where

import Control.Applicative

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
type_check prog = debug prog >> type_check_internal prog
    where debug p = traceM $
            "BOUND!! " ++ show (get_globals p)

type_check_internal :: Program -> Either TypeError CheckedProgram
type_check_internal (Program name imports defns) = do
    Right $ CheckedProgram name imports defns

get_globals :: Program -> Context
get_globals (Program _ imports defns) = Global $
    get_imports imports ++ walk_top_level_statements defns


get_imports :: Imports -> [Bound]
get_imports imports = map make_import_bound (map from_import imports)
    where
        from_import :: Import -> String
        from_import (Import s) = s
        make_import_bound s = Bound (Identifier s) (TypeName "Module")

get_top_level_const_defns_from_prog :: Program -> [Top_Level_Defn]
get_top_level_const_defns_from_prog (Program _ _ defns) = get_top_level_const_defns defns

empty_context :: Context
empty_context = Global []

check_top_level_const_defns :: Top_Level_Defn -> Either TypeError Bound
check_top_level_const_defns stmt = case stmt of
    Top_Level_Const_Defn i Nothing expr -> Right (Bound i (infer empty_context expr))
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

