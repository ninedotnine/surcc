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

type_check :: Program -> Either TypeError CheckedProgram
-- type_check (Program (SoucModule name exports) imports defns) = do
type_check (Program module_info imports defns) = do
    exports_ctx <- case module_info of
        Just (SoucModule _ exports) ->  add_exports exports builtins_ctx
        Nothing -> add_exports [] builtins_ctx
    imports_ctx <- add_imports imports exports_ctx
    case add_globals imports_ctx defns of
        Left err -> Left err
        Right finished_ctx ->
            let undefined_exports = exports_remaining finished_ctx in
                if undefined_exports == []
                    then Right $ CheckedProgram module_info imports defns
                    else Left (ExportedButNotDefined (head undefined_exports))


add_exports :: [ExportDecl] -> Context -> Either TypeError Context
add_exports exports ctx = Right $ Exported (map make_bound exports) ctx
    where
        make_bound (ExportDecl i t) = Bound i (SoucType t)

-- getting imports can fail if (e. g.) a file cannot be found.
-- don't worry about it for now.
-- should also fail if it tries to import something that was
-- declared exported with a non-module type
add_imports :: Imports -> Context -> Either TypeError Context
add_imports imports ctx = Right $ Global (map make_import_bound (map from_import imports)) ctx
    where
        from_import :: Import -> String
        from_import (Import s) = s
        make_import_bound s = Bound (Identifier s) (SoucType "Module")


add_globals :: Context -> [Top_Level_Defn] -> Either TypeError Context
add_globals imports_ctx defns = do
    case runState (run_globals defns) imports_ctx of
        (Right (), ctx) -> (traceM $ "ultimate ctx: " ++ show ctx) >> Right ctx
        (Left e, _) -> Left e


run_globals :: [Top_Level_Defn] -> Checker ()
run_globals defns = do
    let (consts, short_fns, long_fns, routines) = split_top_level_stuff defns
    consts_list <- mapM add_top_level_consts consts
    case check_any_failed consts_list of
        Left err -> pure (Left err)
        Right () -> do
            short_fns_list <- mapM (in_scope add_top_level_short_fns) short_fns
            case check_any_failed short_fns_list of
                Left err -> pure (Left err)
                Right () -> do
                    long_fns_list <- mapM (in_scope add_top_level_long_fns) long_fns
                    case check_any_failed long_fns_list of
                        Left err -> pure (Left err)
                        Right () -> do
                            routines_list <- mapM (in_scope add_top_level_routines) routines
                            pure (check_any_failed routines_list)


add_top_level_consts :: TopLevelConstType -> Checker ()
add_top_level_consts (TopLevelConstType i m_t expr) = do
    ctx <- get
    case m_t of
        Nothing -> case infer ctx expr of
            Right t -> add_potential_export (Bound i t)
            Left err -> pure (Left err)
        Just t -> case check_astree ctx expr t of
            Right () -> add_potential_export (Bound i t)
            Left err -> pure (Left err)

in_scope :: (a -> Checker Bound) -> a -> Checker ()
in_scope act x = do
    new_scope
    result <- act x
    case result of
        Left err -> pure (Left err)
        Right bound -> exit_scope >> insert (BindMayExist False) bound

new_scope :: State Context ()
new_scope = get >>= put . Scoped []

exit_scope :: Checker ()
exit_scope = do
    ctx <- get
    case ctx of
        Scoped _ inner -> put inner >> pure (Right ())
        Global _ _ -> pure (Left (Undeclared "should be unreachable"))
        Exported _ _ -> pure (Left (Undeclared "should be unreachable"))
        Builtins _ -> pure (Left (Undeclared "should be unreachable"))

add_top_level_short_fns :: TopLevelShortFnType -> Checker Bound
add_top_level_short_fns (TopLevelShortFnType i p m_t expr) = do
    ctx <- get
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> case add_bind ctx (BindMayExist False) (Bound param (SoucType p_t)) of
            Left err -> pure (Left err)
            Right p_ctx -> case m_t of
                Nothing -> case infer p_ctx expr of
                    Right t -> pure $ Right (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> pure (Left err)
                Just t -> case check_astree p_ctx expr t of
                    Right () -> pure $ Right (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> pure (Left err)

check_and_bind :: Stmts -> Maybe SoucType -> Bound -> Checker Bound
check_and_bind stmts t bind = do
    res <- check_stmts stmts t
    case res of
        Right () -> pure (Right bind)
        Left err -> pure (Left err)


add_top_level_long_fns :: TopLevelLongFnType -> Checker Bound
add_top_level_long_fns (TopLevelLongFnType i p m_t stmts) = do
    ctx <- get
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> case add_bind ctx (BindMayExist False) (Bound param (SoucType p_t)) of
            Left err -> pure (Left err)
            Right p_ctx -> case m_t of
                Nothing -> case infer_stmts p_ctx stmts of
                    Right t -> pure $ Right (Bound i (SoucFn (SoucType p_t) t))
                    Left err -> pure (Left err)
                Just t -> do
                    put p_ctx
                    check_and_bind stmts (Just t) (Bound i (SoucFn (SoucType p_t) t))

add_top_level_routines :: TopLevelProcType -> Checker Bound
add_top_level_routines (TopLevelProcType i m_p m_t stmts) = case m_t of
    Nothing -> ok_sub
    Just (SoucType "IO") -> ok_sub
    Just wrong -> error (show wrong)
    where
        ok_sub = do
            ctx <- get
            case i of
                "main" -> add_main_routine m_p stmts
                _ -> case m_p of
                    Nothing -> do
                        check_and_bind stmts Nothing (Bound i (SoucRoutn Nothing))
                    Just (Param _ Nothing) -> error "FIXME type inference"
                    Just (Param param (Just p_t)) -> case add_bind ctx (BindMayExist False) (Bound param (SoucType p_t)) of
                        Left err -> pure (Left err)
                        Right p_ctx -> do
                            put p_ctx
                            check_and_bind stmts Nothing (Bound i (SoucRoutn (Just (SoucType p_t))))

add_main_routine :: Maybe Param -> Stmts -> Checker Bound
add_main_routine m_p stmts = case m_p of
    Just (Param _ Nothing) -> error "FIXME infer param type"
    Just (Param p (Just p_t)) -> do
        res <- insert (BindMayExist False) (Bound p (SoucType p_t))
        case res of
            Left err -> pure (Left err)
            Right () -> check_and_bind stmts Nothing (Bound "main" (SoucRoutn (Just (SoucType p_t))))
    Nothing -> check_and_bind stmts Nothing (Bound "main" (SoucRoutn Nothing))


-- check_any_failed :: [Either TypeError ()] -> State Context (Either TypeError ())
check_any_failed :: [Either TypeError ()] -> (Either TypeError ())
check_any_failed list = case lefts list of
    [] -> Right ()
--     (x:_) -> pure $  throwError x
    (x:_) -> Left x



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
