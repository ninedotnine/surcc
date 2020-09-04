{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.TypeChecker (
    type_check,
    check_astree -- for tests
    ) where

import Control.Applicative

import Prelude hiding (lookup)
import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context
import TypeChecker.Operators

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

walk_top_level_statements :: [Top_Level_Defn] -> [Bound]
walk_top_level_statements defns = map unroll defns where
    unroll :: Top_Level_Defn -> Bound
    unroll defn = case defn of
        Top_Level_Const_Defn ident (Just t) _ -> Bound ident t
        FuncDefn ident _ (Just t) _ -> Bound ident ("a -> " <> t)
        FuncDefn ident _ Nothing _ -> Bound ident "a -> b"
        ShortFuncDefn ident _ (Just t) _ -> Bound ident ("Fn a " <> t)
        ShortFuncDefn ident _ Nothing _ -> Bound ident ("Fn a b")
        SubDefn ident _ (Just t) _ -> Bound ident ("What -> " <> t)
        SubDefn ident _ Nothing _ -> Bound ident ("What -> IO")
        _ -> Bound "placeholder_id" "PlaceholderType"
--
--
-- data Top_Level_Defn = Top_Level_Const_Defn Identifier (Maybe TypeName) ASTree
--                     | FuncDefn Identifier Param (Maybe TypeName) Stmts
--                     | ShortFuncDefn Identifier Param (Maybe TypeName) ASTree
--                     | SubDefn Identifier (Maybe Param) (Maybe TypeName) Stmts
--                     | MainDefn (Maybe Param) (Maybe TypeName) Stmts
--                     deriving (Show, Eq)

infer :: Context -> ASTree -> TypeName
infer ctx tree = case tree of
    Branch op left right -> t where (Arg _, Arg _, Ret t) = infer_infix_op op left right
    Twig op expr -> t where (Arg _, Ret t) = infer_prefix_op op expr
    Signed expr _ -> infer ctx expr
    Leaf term -> infer_term ctx term

infer_term :: Context -> Term -> TypeName
infer_term context term = case term of
    LitInt _    -> (TypeName "Integer")
    LitChar _   -> (TypeName "Char")
    LitBool _   -> (TypeName "Bool")
    LitString _ -> (TypeName "String")
    Var v -> case lookup context v of
        Nothing -> TypeName "FIXME OhNOOO"
        Just t -> t

check_equals :: TypeName -> TypeName -> Maybe TypeError
check_equals t0 t1 = if t0 == t1 then Nothing else Just (TypeError t0 t1)

check_astree :: Context -> ASTree -> TypeName -> Maybe TypeError
check_astree ctx tree t = case tree of
    Branch op left right -> check_astree ctx left l_t
                        <|> check_astree ctx right r_t
                        <|> check_equals t expr_t
        where (Arg l_t, Arg r_t, Ret expr_t) = infer_infix_op op left right

    Twig op expr -> check_astree ctx expr arg_t
                <|> check_equals t expr_t
        where (Arg arg_t, Ret expr_t) = infer_prefix_op op expr

    Leaf term -> check_equals t term_t
        where term_t = infer_term ctx term

    Signed expr sig ->  check_astree ctx expr expr_t
                    <|> check_equals sig expr_t
                    <|> check_equals t expr_t
        where expr_t = infer ctx expr
