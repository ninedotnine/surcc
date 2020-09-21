{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.TypeChecker (
    type_check,
    add_globals, -- for tests
    add_imports, -- for tests
    ) where

import Control.Applicative
import Control.Monad.State
import Data.Either

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
        (Right (), ctx) -> (traceM $ "ultimate ctx: " ++ show ctx) >> Right ctx
        (Left e, _) -> Left e


type Checker a = State Context (Either TypeError a)

run_globals :: [Top_Level_Defn] -> Checker ()
run_globals defns = do
    let (consts, short_fns, long_fns, routines) = split_top_level_stuff defns
    consts_list <- mapM add_top_level_consts consts
    case check_any_failed consts_list of
        Left err -> return (Left err)
        Right () -> do
            short_fns_list <- mapM (in_scope add_top_level_short_fns) short_fns
            case check_any_failed short_fns_list of
                Left err -> return (Left err)
                Right () -> do
                    long_fns_list <- mapM (in_scope add_top_level_long_fns) long_fns
                    return (check_any_failed long_fns_list)

add_top_level_consts :: TopLevelConstType -> Checker ()
add_top_level_consts (TopLevelConstType i m_t expr) = do
    ctx <- get
    case m_t of
        Nothing -> case infer ctx expr of
            Right t -> insert (Bound i t)
            Left err -> return (Left err)
        Just t -> case check_astree ctx expr t of
            Right () -> insert (Bound i t)
            Left err -> return (Left err)

in_scope :: (a -> Checker Bound) -> a -> Checker ()
in_scope act x = do
    ctx <- get
    new_scope
    result <- act x
    case result of
        Left err -> return (Left err)
        Right bound -> exit_scope >> case add_bind ctx bound of
            Left err -> return (Left err)
            Right new_ctx -> put new_ctx >> return (Right ())

new_scope :: State Context ()
new_scope = get >>= put . Scoped []

exit_scope :: Checker ()
exit_scope = do
    ctx <- get
    case ctx of
        Scoped _ inner -> put inner >> return (Right ())
        Global _ -> return (Left (Undeclared "should be unreachable"))


add_top_level_short_fns :: TopLevelShortFnType -> Checker Bound
add_top_level_short_fns (TopLevelShortFnType i p m_t expr) = do
    ctx <- get
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> case add_bind ctx (Bound param (SoucType p_t)) of
            Left err -> return (Left err)
            Right p_ctx -> case m_t of
                Nothing -> case infer p_ctx expr of
                    Right t -> return $ Right (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> return (Left err)
                Just t -> case check_astree p_ctx expr t of
                    Right () -> return $ Right (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> return (Left err)

add_top_level_long_fns :: TopLevelLongFnType -> Checker Bound
add_top_level_long_fns (TopLevelLongFnType i p m_t stmts) = do
    ctx <- get
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> case add_bind ctx (Bound param (SoucType p_t)) of
            Left err -> return (Left err)
            Right p_ctx -> case m_t of
                Nothing -> case infer_stmts p_ctx stmts of
                    Right t -> return $ Right (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> return (Left err)
                Just t -> case check_stmts p_ctx stmts (Just t) of
                    Right () -> return $ Right (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> return (Left err)


-- check_any_failed :: [Either TypeError ()] -> State Context (Either TypeError ())
check_any_failed :: [Either TypeError ()] -> (Either TypeError ())
check_any_failed list = case lefts list of
    [] -> Right ()
--     (x:_) -> return $  throwError x
    (x:_) -> Left x


insert :: Bound -> Checker ()
insert bound = do
    traceM $ "inserting: " ++ show bound
    ctx <- get
    case (add_bind ctx bound) of
        Left err -> return (Left err)
        Right new_ctx -> put new_ctx >> return (Right ())


type BrokenUpList = ([TopLevelConstType], [TopLevelShortFnType], [TopLevelLongFnType], [TopLevelProcType])

split_top_level_stuff :: [Top_Level_Defn] -> BrokenUpList
split_top_level_stuff defns = reverse_all (split_top_level_stuff_rec ([], [], [], []) defns)
    where
        reverse_all (ws, xs, ys, zs) = (reverse ws, reverse xs, reverse ys, reverse zs)

split_top_level_stuff_rec :: BrokenUpList -> [Top_Level_Defn] -> BrokenUpList
split_top_level_stuff_rec lists [] = lists
split_top_level_stuff_rec (ws, xs, ys, zs) (d:defns) = case d of
    Top_Level_Const_Defn i m_t expr -> split_top_level_stuff_rec (TopLevelConstType i (SoucType <$> m_t) expr:ws, xs, ys, zs) defns
    ShortFuncDefn i p m_t expr ->      split_top_level_stuff_rec (ws, TopLevelShortFnType i p (SoucType <$> m_t) expr:xs, ys, zs) defns
    FuncDefn i p m_t stmts ->          split_top_level_stuff_rec (ws, xs, TopLevelLongFnType i p (SoucType <$> m_t) stmts:ys, zs) defns
    SubDefn i m_p m_t stmts ->         split_top_level_stuff_rec (ws, xs, ys, TopLevelProcType i m_p (SoucType <$> m_t) stmts:zs) defns
    MainDefn m_p m_t stmts ->          split_top_level_stuff_rec (ws, xs, ys, TopLevelProcType "main" m_p (SoucType <$> m_t) stmts:zs) defns


data TopLevelConstType = TopLevelConstType Identifier (Maybe SoucType) ASTree
    deriving (Show, Eq)
data TopLevelShortFnType = TopLevelShortFnType Identifier Param (Maybe SoucType) ASTree
    deriving (Show, Eq)
data TopLevelLongFnType = TopLevelLongFnType Identifier Param (Maybe SoucType) Stmts
    deriving (Show, Eq)
data TopLevelProcType = TopLevelProcType Identifier (Maybe Param) (Maybe SoucType) Stmts
    deriving (Show, Eq)
