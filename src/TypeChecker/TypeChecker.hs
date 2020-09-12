{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.TypeChecker (
    type_check,
    add_globals, -- for tests
    add_imports, -- for tests
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Either
import Data.Maybe (catMaybes)

import Prelude hiding (lookup)
import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context
import TypeChecker.Operators
import TypeChecker.Expressions
import TypeChecker.Statements

import Debug.Trace

type_check :: Program -> Either TypeError CheckedProgram
type_check (Program name imports defns) = do
    imports_ctx <- add_imports imports
    case add_globals imports_ctx defns of
        Left err -> Left err
        Right _ -> Right $ CheckedProgram name imports defns


-- getting imports can fail if (e. g.) a file cannot be found.
-- don't worry about it for now.
add_imports :: Imports -> Either TypeError Context
add_imports imports = Right $ Global $ map make_import_bound (map from_import imports)
    where
        from_import :: Import -> String
        from_import (Import s) = s
        make_import_bound s = Bound (Identifier s) (SoucType "Module")


add_globals :: Context -> [Top_Level_Defn] -> Either TypeError Context
add_globals imports_ctx defns = do
    case runState (run_globals defns) imports_ctx of
        (Nothing, ctx) -> (traceM $ "ultimate ctx: " ++ show ctx) >> Right ctx
        (Just e, _) -> Left e

run_globals :: [Top_Level_Defn] -> State Context (Maybe TypeError)
run_globals defns = do
    let (consts, short_fns, long_fns, routines) = split_top_level_stuff defns
    consts_list <- mapM add_top_level_consts consts
    case check_any_failed consts_list of
        Just err -> return (Just err)
        Nothing -> do
            short_fns_list <- mapM add_top_level_short_fns short_fns
            case check_any_failed short_fns_list of
                Just err -> return (Just err)
                Nothing -> do
                    long_fns_list <- mapM add_top_level_long_fns long_fns
                    return (check_any_failed long_fns_list)

add_top_level_consts :: TopLevelConstType -> State Context (Maybe TypeError)
add_top_level_consts (TopLevelConstType i m_t expr) = do
    ctx <- get
    case m_t of
        Nothing -> case infer ctx expr of
            Right t -> insert (Bound i t)
            Left err -> return (Just err)
        Just t -> case check_astree ctx expr (SoucType t) of
            Right () -> insert (Bound i (SoucType t))
            Left err -> return (Just err)

add_top_level_short_fns :: TopLevelShortFnType -> State Context (Maybe TypeError)
add_top_level_short_fns (TopLevelShortFnType i p m_t expr) = do
    ctx <- get
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> case add_bind ctx (Bound param (SoucType p_t)) of
            Left err -> return (Just err)
            Right p_ctx -> case m_t of
                Nothing -> case infer p_ctx expr of
                    Right t -> insert (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> return (Just err)
                Just t -> case check_astree p_ctx expr (SoucType t) of
                    Right () -> insert (Bound i (SoucFn (SoucType p_t) (SoucType t)))
                    Left err -> return (Just err)

add_top_level_long_fns :: TopLevelLongFnType -> State Context (Maybe TypeError)
add_top_level_long_fns (TopLevelLongFnType i p m_t stmts) = do
    ctx <- get
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> case add_bind ctx (Bound param (SoucType p_t)) of
            Left err -> return (Just err)
            Right p_ctx -> case m_t of
                Nothing -> case infer_stmts p_ctx stmts of
                    Right t -> insert (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> return (Just err)
                Just t -> case check_stmts p_ctx stmts (Just (SoucType t)) of
                    Right () -> insert (Bound i (SoucFn (SoucType p_t) (SoucType t)))
                    Left err -> return (Just err)


check_any_failed :: [Maybe TypeError] -> Maybe TypeError
check_any_failed list = case catMaybes list of
    [] -> Nothing
    (x:_) -> Just x


insert :: Bound -> State Context (Maybe TypeError)
insert bound = do
    traceM $ "inserting: " ++ show bound
    ctx <- get
    case (add_bind ctx bound) of
        Left err -> return (Just err)
        Right new_ctx -> put new_ctx >> return Nothing


type BrokenUpList = ([TopLevelConstType], [TopLevelShortFnType], [TopLevelLongFnType], [TopLevelProcType])

split_top_level_stuff :: [Top_Level_Defn] -> BrokenUpList
split_top_level_stuff defns = reverse_all (split_top_level_stuff_rec ([], [], [], []) defns)
    where
        reverse_all (ws, xs, ys, zs) = (reverse ws, reverse xs, reverse ys, reverse zs)

split_top_level_stuff_rec :: BrokenUpList -> [Top_Level_Defn] -> BrokenUpList
split_top_level_stuff_rec lists [] = lists
split_top_level_stuff_rec (ws, xs, ys, zs) (d:defns) = case d of
    Top_Level_Const_Defn i m_t expr -> split_top_level_stuff_rec (TopLevelConstType i m_t expr:ws, xs, ys, zs) defns
    ShortFuncDefn i p m_t expr ->      split_top_level_stuff_rec (ws, TopLevelShortFnType i p m_t expr:xs, ys, zs) defns
    FuncDefn i p m_t stmts ->          split_top_level_stuff_rec (ws, xs, TopLevelLongFnType i p m_t stmts:ys, zs) defns
    SubDefn i m_p m_t stmts ->         split_top_level_stuff_rec (ws, xs, ys, TopLevelProcType i m_p m_t stmts:zs) defns
    MainDefn m_p m_t stmts ->          split_top_level_stuff_rec (ws, xs, ys, TopLevelProcType "main" m_p m_t stmts:zs) defns


data TopLevelConstType = TopLevelConstType Identifier (Maybe TypeName) ASTree
    deriving (Show, Eq)
data TopLevelShortFnType = TopLevelShortFnType Identifier Param (Maybe TypeName) ASTree
    deriving (Show, Eq)
data TopLevelLongFnType = TopLevelLongFnType Identifier Param (Maybe TypeName) Stmts
    deriving (Show, Eq)
data TopLevelProcType = TopLevelProcType Identifier (Maybe Param) (Maybe TypeName) Stmts
    deriving (Show, Eq)
