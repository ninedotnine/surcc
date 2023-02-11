{-# LANGUAGE FlexibleContexts #-}

module SurCC.TypeChecker.TypeChecker (
    type_check,
    add_globals, -- for tests
    add_imports, -- for tests
    ) where

import Control.Applicative ()
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except ()
import Control.Monad.Trans ()
import Control.Monad.Trans.Except (runExceptT, throwE)
import Data.List (null, (\\))
import Data.Traversable (traverse)
import Data.Function ((&))
import Data.Functor

import Data.Either ()
import Data.Text (Text)
import TextShow (TextShow(..), toString) -- for error
import Data.Text qualified as Text

import Prelude hiding (lookup)
import SurCC.Common
import SurCC.TypeChecker.Context (Checker,
                            ExportList,
                            ImportList,
                            LocalScope,
                            run_checker,
                            insert_global,
                            new_scope,
                            new_param_scope,
                            new_main_scope,
                            import_list,
                            export_list,
                            exit_scope,
                           )
import SurCC.TypeChecker.Expressions
import SurCC.TypeChecker.Statements
import SurCC.TypeChecker.Typedefs (build_typedefs)


type_check :: ParseTree -> Either TypeError CheckedProgram
-- fixme: use the typedefs
type_check (ParseTree module_info imports typedefs defns) = do
    exports_list <- add_exports module_info
    (_types,_typedefs_ctx) <- build_typedefs typedefs exports_list
    imports_list <- add_imports imports exports_list
    globals <- add_globals defns imports_list exports_list
    -- FIXME: top level first, then sub-trees:
--     top_level_ctx <- add_globals imports_ctx defns
--     finished_ctx <- check_subtrees top_level_ctx defns
    -- this could be done before subtrees?
    assert_no_undefined_exports globals module_info
    Right $ CheckedProgram module_info imports defns


assert_no_undefined_exports :: (MonadError TypeError m)
                               => [Bound] -> SurCModule -> m ()
assert_no_undefined_exports defined (SurCModule _ exports) = do
    unless (null (exported \\ defined)) $
        throwError (ExportedButNotDefined (head exported))
    where
        unwrap (ExportDecl b) = b
        exported = exports <&> unwrap


-- this returns an Either TypeError ExportList because it could fail.
-- e. g. the same name could be exported multiple times, possibly with
-- different types even.
-- don't worry about it for now.
add_exports :: SurCModule -> Either TypeError ExportList
add_exports (SurCModule _ exports) = Right $ export_list exports

-- getting imports can fail if (e. g.) a file cannot be found.
-- don't worry about it for now.
-- FIXME
-- should also fail if it tries to import something that was
-- declared exported with a non-module type
add_imports :: [ImportDecl] -> ExportList -> Either TypeError ImportList
add_imports imports _exports = Right $ import_list imports

add_globals :: [TopLevelDefn] -> ImportList -> ExportList
               -> Either TypeError [Bound]
add_globals defns imports exports =
    run_checker imports exports (run_globals defns)

run_globals :: [TopLevelDefn] -> Checker [Bound]
run_globals defns = traverse add_top_level_defns defns


-- FIXME bind all the top-level things first, then go into the subtrees
add_top_level_defns :: TopLevelDefn -> Checker Bound
add_top_level_defns = \case
    TopLevelConstDefn i m_t expr -> add_top_level_const i m_t expr
    FuncDefn i p m_t stmts -> add_top_level_long_fn i p m_t stmts
    ShortFuncDefn i p m_t expr -> add_top_level_short_fn i p m_t expr
    SubDefn i m_p m_t stmts -> add_top_level_sub i m_p m_t stmts
    MainDefn p m_t stmts -> add_main_routine p m_t stmts


add_top_level_const :: Identifier -> Maybe SoucType -> ExprTree ->
                       Checker Bound
add_top_level_const i m_t expr = do
    t <- infer_if_needed m_t expr
    insert_global $ Bound i t
    pure $ Bound i t


add_top_level_short_fn :: Identifier -> Param -> Maybe SoucType -> ExprTree
                        -> Checker Bound
add_top_level_short_fn i p m_t expr = case p of
    Param _ Nothing -> error "FIXME type inference"
    Param param (Just p_t) -> do
        new_param_scope param p_t
        t <- infer_if_needed m_t expr
        exit_scope
        insert_global $ Bound i (SoucFn p_t t)
        pure $ Bound i (SoucFn p_t t)


add_top_level_long_fn :: Identifier -> Param -> Maybe SoucType -> Stmts
                        -> Checker Bound
add_top_level_long_fn i p m_t stmts = case p of
    Param _ Nothing -> error "FIXME type inference"
    Param param (Just p_t) -> do
        new_param_scope param p_t
        case m_t of
            Nothing -> do
                t <- infer_stmts stmts
                exit_scope
                insert_global (Bound i (SoucFn p_t t))
                pure (Bound i (SoucFn p_t t))
            Just t -> do
                check_stmts t stmts
                exit_scope
                insert_global (Bound i (SoucFn p_t t))
                pure (Bound i (SoucFn p_t t))

add_top_level_sub :: Identifier -> Maybe Param -> Maybe SoucType -> Stmts
                    -> Checker Bound
add_top_level_sub i m_p m_t stmts = case (i, m_t) of
    ("main", _) -> error "tried to add \"main\" as a subroutine"
    (_, Nothing) -> ok_sub
    (_, Just SoucIO) -> ok_sub
    (_, Just wrong) -> error (toString (showb wrong))
    where
        ok_sub = case m_p of
            Nothing -> do
                new_scope
                check_stmts SoucIO stmts
                exit_scope
                insert_global (Bound i SoucIO)
                pure (Bound i SoucIO)
            Just (Param _ Nothing) -> error "FIXME type inference"
            Just (Param param (Just p_t)) -> do
                new_param_scope param p_t
                check_stmts SoucIO stmts
                exit_scope
                insert_global (Bound i (SoucRoutn p_t))
                pure (Bound i (SoucRoutn p_t))


add_main_routine :: MainParam -> Maybe SoucType -> Stmts -> Checker Bound
-- FIXME
-- main can have many different types,
-- depending on which parameter is used
add_main_routine param _m_t stmts = do
    -- FIXME
    -- if a type annotation is given,
    -- it should be checked with the type
    -- that we know the main param must have
    new_main_scope param
    check_stmts SoucIO stmts
    exit_scope
    pure $ Bound "main" (SoucRoutn (SoucType "OutputStream" (SoucKind 0)))
