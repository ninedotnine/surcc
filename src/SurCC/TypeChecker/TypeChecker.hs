module SurCC.TypeChecker.TypeChecker (
    type_check,
    add_globals, -- for tests
    add_imports, -- for tests
    ) where

import Control.Applicative ()
import Control.Monad.State (runState, get)
import Control.Monad.Except ()
import Control.Monad.Trans ()
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.Either ()
import Data.Text (Text)
import TextShow (TextShow(..), toString) -- for error
import Data.Text qualified as Text

import Prelude hiding (lookup)
import SurCC.Common
import SurCC.TypeChecker.Context (Checker,
                            ExportList(..),
                            LocalScope,
                            make_export_list,
                            make_global_scope,
                            undefined_export,
                            add_potential_export,
                            new_scope,
                            new_param_scope,
                            exit_scope,
                           )
import SurCC.TypeChecker.Expressions
import SurCC.TypeChecker.Statements

type_check :: ParseTree -> Either TypeError CheckedProgram
-- fixme: use the typedefs
type_check (ParseTree module_info imports _ defns) = do
    exports_ctx <- add_exports module_info
    imports_ctx <- add_imports imports exports_ctx
    finished_ctx <- add_globals imports_ctx defns
    case undefined_export finished_ctx of
        Nothing -> Right $ CheckedProgram module_info imports defns
        Just export -> Left (ExportedButNotDefined export)

-- this returns an Either TypeError ExportList because it could fail.
-- e. g. the same name could be exported multiple times, possibly with
-- different types even.
-- don't worry about it for now.
add_exports :: SurCModule -> Either TypeError ExportList
add_exports (SurCModule _ exports) = Right $ make_export_list exports

-- getting imports can fail if (e. g.) a file cannot be found.
-- don't worry about it for now.
-- should also fail if it tries to import something that was
-- declared exported with a non-module type
add_imports :: Imports -> ExportList -> Either TypeError LocalScope
add_imports imports ctx = Right $ make_global_scope imports ctx

add_globals :: LocalScope -> [TopLevelDefn] -> Either TypeError LocalScope
add_globals imports_ctx defns =
    case runState (runExceptT (run_globals defns)) imports_ctx of
        (Right (), ctx) -> Right ctx
        (Left e, _) -> Left e

run_globals :: [TopLevelDefn] -> Checker ()
run_globals defns = mapM_ add_top_level_defns defns

add_top_level_defns :: TopLevelDefn -> Checker ()
add_top_level_defns = \case
    TopLevelConstDefn i m_t expr -> add_top_level_const i m_t expr
    FuncDefn i p m_t stmts -> add_top_level_long_fn i p m_t stmts
    ShortFuncDefn i p m_t expr -> add_top_level_short_fn i p m_t expr
    SubDefn i m_p m_t stmts -> add_top_level_sub i m_p m_t stmts
    MainDefn m_p m_t stmts -> add_main_routine m_p m_t stmts


add_top_level_const :: Identifier -> Maybe SoucType -> ExprTree -> Checker ()
add_top_level_const i m_t expr = do
    t <- infer_if_needed m_t expr
    add_potential_export $ bound_id i t


add_top_level_short_fn :: Identifier -> Param -> Maybe SoucType -> ExprTree
                        -> Checker ()
add_top_level_short_fn i p m_t expr = do
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> do
            new_param_scope param p_t
            t <- infer_if_needed m_t expr
            exit_scope
            add_potential_export $ bound_id i (SoucFn p_t t)


add_top_level_long_fn :: Identifier -> Param -> Maybe SoucType -> Stmts
                        -> Checker ()
add_top_level_long_fn i p m_t stmts = do
    case p of
        Param _ Nothing -> error "FIXME type inference"
        Param param (Just p_t) -> do
            new_param_scope param p_t
            p_ctx <- get
            case m_t of
                Nothing -> case infer_stmts p_ctx stmts of
                    Right t -> do
                        exit_scope
                        add_potential_export (bound_id i (SoucFn p_t t))
                    Left err -> throwE err
                Just t -> do
                    check_stmts stmts (Just t)
                    exit_scope
                    add_potential_export (bound_id i (SoucFn p_t t))

add_top_level_sub :: Identifier -> Maybe Param -> Maybe SoucType -> Stmts
                    -> Checker ()
add_top_level_sub i m_p m_t stmts = case (i, m_t) of
    ("main", _) -> error "tried to add \"main\" as a subroutine"
    (_, Nothing) -> ok_sub
    (_, Just SoucIO) -> ok_sub
    (_, Just wrong) -> error (toString (showb wrong))
    where
        ok_sub = case m_p of
            Nothing -> do
                new_scope
                check_stmts stmts Nothing
                exit_scope
                add_potential_export (bound_id i SoucIO)
            Just (Param _ Nothing) -> error "FIXME type inference"
            Just (Param param (Just p_t)) -> do
                new_param_scope param p_t
                check_stmts stmts Nothing
                exit_scope
                add_potential_export (bound_id i (SoucRoutn p_t))

add_main_routine :: Maybe Param -> Maybe SoucType -> Stmts -> Checker ()
add_main_routine m_p m_t stmts = case m_p of
    Just (Param _ Nothing) -> error "FIXME infer param type"
    Just (Param p (Just p_t)) -> do
        new_param_scope p p_t
        check_stmts stmts m_t
        exit_scope
    Nothing -> do
        new_scope
        check_stmts stmts m_t
        exit_scope
