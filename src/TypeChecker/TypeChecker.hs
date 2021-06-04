module TypeChecker.TypeChecker (
    type_check,
    add_globals, -- for tests
    add_imports, -- for tests
    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Data.Either
import Data.Text (Text)
import qualified Data.Text as Text

import Prelude hiding (lookup)
import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context
import TypeChecker.Operators
import TypeChecker.Expressions
import TypeChecker.Statements

type_check :: ParseTree -> Either TypeError CheckedProgram
type_check (ParseTree module_info imports defns) = do
    exports_ctx <- add_exports module_info
    imports_ctx <- add_imports imports exports_ctx
    finished_ctx <- add_globals imports_ctx defns
    let undefined_exports = exports_remaining finished_ctx in
        if undefined_exports == []
            then Right $ CheckedProgram module_info imports defns
            else Left (ExportedButNotDefined (head undefined_exports))


add_exports :: SoucModule -> Either TypeError ExportList
add_exports (SoucModule _ exports) = Right $ ExportList (map make_bound exports)
    where
        make_bound (ExportDecl i t) = Bound i t

-- getting imports can fail if (e. g.) a file cannot be found.
-- don't worry about it for now.
-- should also fail if it tries to import something that was
-- declared exported with a non-module type
add_imports :: Imports -> ExportList -> Either TypeError LocalScope
add_imports imports ctx = Right $ GlobalScope (map make_import_bound (map from_import imports)) ctx
    where
        from_import :: ImportDecl -> Text
        from_import = \case
            LibImport s -> Text.pack s
            RelImport s -> Text.pack s
        make_import_bound s = Bound (Identifier s) (SoucType "Module")


add_globals :: LocalScope -> [Top_Level_Defn] -> Either TypeError LocalScope
add_globals imports_ctx defns =
    case runState (runExceptT (run_globals defns)) imports_ctx of
        (Right (), ctx) -> Right ctx
        (Left e, _) -> Left e


run_globals :: [Top_Level_Defn] -> Checker ()
run_globals defns = do
    let (consts, short_fns, long_fns, routines) = split_top_level_stuff defns
    mapM_ add_top_level_consts consts
    mapM_ (in_scope add_top_level_short_fns) short_fns
    mapM_ (in_scope add_top_level_long_fns) long_fns
    mapM_ (in_scope add_top_level_routines) routines
    pure ()


add_top_level_consts :: TopLevelConstType -> Checker ()
add_top_level_consts (TopLevelConstType i m_t expr) = do
    t <- infer_if_needed m_t expr
    add_potential_export (Bound i t)


in_scope :: (a -> Checker Bound) -> a -> Checker ()
in_scope act x = do
    new_scope
    bound <- act x
    exit_scope >> add_potential_export bound

new_scope :: Checker ()
new_scope = get >>= put . InnerScope []

exit_scope :: Checker ()
exit_scope = do
    ctx <- get
    case ctx of
        InnerScope _ inner -> put inner >> pure ()
        GlobalScope _ _ -> throwE (Undeclared "should be unreachable")


add_top_level_short_fns :: TopLevelShortFnType -> Checker Bound
add_top_level_short_fns (TopLevelShortFnType i p m_t expr) = do
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> do
            insert_param param p_t
            t <- infer_if_needed m_t expr
            pure (Bound i (SoucFn p_t t))


check_and_bind :: Stmts -> Maybe SoucType -> Bound -> Checker Bound
check_and_bind stmts t bind = do
    check_stmts stmts t
    pure bind


add_top_level_long_fns :: TopLevelLongFnType -> Checker Bound
add_top_level_long_fns (TopLevelLongFnType i p m_t stmts) = do
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> do
            insert_param param p_t
            p_ctx <- get
            case m_t of
                Nothing -> case infer_stmts p_ctx stmts of
                    Right t -> pure (Bound i (SoucFn p_t t))
                    Left err -> throwE err
                Just t -> do
                    check_and_bind stmts (Just t) (Bound i (SoucFn p_t t))

add_top_level_routines :: TopLevelProcType -> Checker Bound
add_top_level_routines (TopLevelProcType i m_p m_t stmts) = case m_t of
    Nothing -> ok_sub
    Just (SoucType "IO") -> ok_sub
    Just wrong -> error (show wrong)
    where
        ok_sub = case i of
            "main" -> add_main_routine m_p stmts
            _ -> case m_p of
                Nothing -> do
                    check_and_bind stmts Nothing (Bound i (SoucType "IO"))
                Just (Param _ Nothing) -> error "FIXME type inference"
                Just (Param param (Just p_t)) -> do
                    insert_param param p_t
                    check_and_bind stmts Nothing (Bound i (SoucRoutn (Just p_t)))

add_main_routine :: Maybe Param -> Stmts -> Checker Bound
add_main_routine m_p stmts = case m_p of
    Just (Param _ Nothing) -> error "FIXME infer param type"
    Just (Param p (Just p_t)) -> do
        insert_global (Bound p p_t)
        check_and_bind stmts Nothing (Bound "main" (SoucRoutn (Just p_t)))
    Nothing -> check_and_bind stmts Nothing (Bound "main" (SoucType "IO"))


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


data TopLevelConstType = TopLevelConstType Identifier (Maybe SoucType) ASTree
    deriving (Show, Eq)
data TopLevelShortFnType = TopLevelShortFnType Identifier Param (Maybe SoucType) ASTree
    deriving (Show, Eq)
data TopLevelLongFnType = TopLevelLongFnType Identifier Param (Maybe SoucType) Stmts
    deriving (Show, Eq)
data TopLevelProcType = TopLevelProcType Identifier (Maybe Param) (Maybe SoucType) Stmts
    deriving (Show, Eq)
