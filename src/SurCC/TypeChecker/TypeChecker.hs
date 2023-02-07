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
import Data.Function ((&))
import Data.Functor

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
                            new_main_scope,
                            exit_scope,
                           )
import SurCC.TypeChecker.Expressions
import SurCC.TypeChecker.Statements
import SurCC.TypeChecker.Typedefs (build_typedefs)

import Debug.Trace

type_check :: ParseTree -> Either TypeError CheckedProgram
-- fixme: use the typedefs
type_check (ParseTree module_info imports typedefs defns) = do
    exports_ctx <- add_exports module_info
    (_types,typedefs_ctx) <- build_typedefs typedefs exports_ctx
--     traceM $ "types is " <> show types
    traceM $ "typedefs is " <> show typedefs_ctx
    imports_ctx <- add_imports imports exports_ctx
    finished_ctx <- add_globals imports_ctx defns
    -- FIXME: top level first, then sub-trees:
--     top_level_ctx <- add_globals imports_ctx defns
--     finished_ctx <- check_subtrees top_level_ctx defns
    -- this could be done before subtrees?
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


-- FIXME bind all the top-level things first, then go into the subtrees
add_top_level_defns :: TopLevelDefn -> Checker ()
add_top_level_defns = \case
    TopLevelConstDefn i m_t expr -> add_top_level_const i m_t expr
    FuncDefn i p m_t stmts -> add_top_level_long_fn i p m_t stmts
    ShortFuncDefn i p m_t expr -> add_top_level_short_fn i p m_t expr
    SubDefn i m_p m_t stmts -> add_top_level_sub i m_p m_t stmts
    MainDefn p m_t stmts -> add_main_routine p m_t stmts


add_top_level_const :: Identifier -> Maybe SoucType -> ExprTree -> Checker ()
add_top_level_const i m_t expr = do
    t <- infer_if_needed m_t expr
    add_potential_export $ Bound i t


add_top_level_short_fn :: Identifier -> Param -> Maybe SoucType -> ExprTree
                        -> Checker ()
add_top_level_short_fn i p m_t expr = case p of
    Param _ Nothing -> error "FIXME type inference"
    Param param (Just p_t) -> do
        new_param_scope param p_t
        t <- infer_if_needed m_t expr
        exit_scope
        add_potential_export $ Bound i (SoucFn p_t t)


add_top_level_long_fn :: Identifier -> Param -> Maybe SoucType -> Stmts
                        -> Checker ()
add_top_level_long_fn i p m_t stmts = case p of
    Param _ Nothing -> error "FIXME type inference"
    Param param (Just p_t) -> do
        new_param_scope param p_t
        case m_t of
            Nothing -> case infer_stmts stmts of
                Right t -> do
                    exit_scope
                    add_potential_export (Bound i (SoucFn p_t t))
                Left err -> throwE err
            Just t -> do
                check_stmts stmts (Just t)
                exit_scope
                add_potential_export (Bound i (SoucFn p_t t))

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
                add_potential_export (Bound i SoucIO)
            Just (Param _ Nothing) -> error "FIXME type inference"
            Just (Param param (Just p_t)) -> do
                new_param_scope param p_t
                check_stmts stmts Nothing
                exit_scope
                add_potential_export (Bound i (SoucRoutn p_t))


add_main_routine :: MainParam -> Maybe SoucType -> Stmts -> Checker ()
add_main_routine param m_t stmts = do
    -- FIXME
    -- if a type annotation is given,
    -- it should be checked with the type
    -- that we know the main param must have
    new_main_scope param
    check_stmts stmts m_t
    exit_scope
