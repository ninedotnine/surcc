{-# LANGUAGE FlexibleContexts #-}

module SurCC.TypeChecker.TypeChecker (
    type_check,
    ) where

import Control.Applicative ()
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except ()
import Control.Monad.Trans ()
import Data.List ((\\), nub)
import Data.Function ((&))
import Data.Functor

import Data.Either ()
import TextShow (TextShow(..), toString) -- for error

import Prelude hiding (lookup)
import SurCC.Common
import SurCC.TypeChecker.Context (
                            TopChecker,
                            ExportList,
                            ImportList,
                            GlobalScope,
                            run_top_checker,
                            insert_global,
                            local_scope,
                            local_scope_param,
                            local_scope_main,
                            import_list,
                            export_list,
                           )
import SurCC.TypeChecker.Expressions (checkm_expr)
import SurCC.TypeChecker.Statements (infer_stmts, check_stmts, checkm_stmts)
import SurCC.TypeChecker.Typedefs (build_typedefs)


-- FIXME: add the module name to the global scope
type_check :: ParseTree -> Either TypeError CheckedProgram
type_check (ParseTree module_info imports typedefs defns) = do
    exports_list <- add_exports module_info
    (types_ctx,typedef_bounds) <- build_typedefs typedefs exports_list
    imports_list <- add_imports imports exports_list
    globals <- run_top_checker imports_list exports_list types_ctx $
                    traverse add_top_level_defn defns

    -- FIXME: top level first, then sub-trees:
--     top_level_ctx <- add_globals imports_ctx defns
--     finished_ctx <- check_subtrees top_level_ctx defns
    -- this could be done before subtrees?
    let all_bounds = nub (globals <> typedef_bounds)
    assert_no_undefined_exports all_bounds module_info
    Right $ CheckedProgram module_info imports defns


assert_no_undefined_exports :: (MonadError TypeError m)
                               => [Bound] -> SurCModule -> m ()
assert_no_undefined_exports defined (SurCModule _ exports) = do
    unless (null undefined_exports) $
        throwError (ExportedButNotDefined (head undefined_exports))
    where
        unwrap (ExportDecl b) = b
        exported = exports <&> unwrap
        undefined_exports = exported \\ defined

-- this returns an Either TypeError ExportList because it could fail.
-- e. g. the same name could be exported multiple times, possibly with
-- different types even.
-- don't worry about it for now.
add_exports :: MonadError TypeError m => SurCModule -> m ExportList
add_exports (SurCModule _ exports) = pure $ export_list exports

-- getting imports can fail if (e. g.) a file cannot be found.
-- don't worry about it for now.
-- FIXME
-- should also fail if it tries to import something that was
-- declared exported with a non-module type
add_imports :: MonadError TypeError m => [ImportDecl] -> ExportList
            -> m ImportList
add_imports imports _exports = pure $ import_list imports



-- FIXME bind all the top-level things first, then go into the subtrees
add_top_level_defn :: TopLevelDefn -> TopChecker Bound
add_top_level_defn = \case
    TopLevelConstDefn i m_t expr -> add_top_level_const i m_t expr
    FuncDefn i p m_t stmts -> add_top_level_long_fn i p m_t stmts
    ShortFuncDefn i p m_t expr -> add_top_level_short_fn i p m_t expr
    SubDefn i m_p m_t stmts -> add_top_level_sub i m_p m_t stmts
    MainDefn p m_t stmts -> add_main_routine p m_t stmts


add_top_level_const :: Identifier -> Maybe SoucType -> ExprTree ->
                       TopChecker Bound
add_top_level_const i m_t expr = do
    t <- local_scope (checkm_expr m_t expr)
    insert_global $ Bound i t
    pure $ Bound i t


add_top_level_short_fn :: Identifier -> Param -> Maybe SoucType -> ExprTree
                        -> TopChecker Bound
add_top_level_short_fn i p m_t expr = case p of
    Param _ Nothing -> error "FIXME type inference"
    Param param (Just p_t) -> do
        t <- local_scope_param param p_t (checkm_expr m_t expr)
        insert_global $ Bound i (SoucFn p_t t)
        pure $ Bound i (SoucFn p_t t)


add_top_level_long_fn :: Identifier -> Param -> Maybe SoucType -> Stmts
                        -> TopChecker Bound
add_top_level_long_fn i p m_t stmts = case p of
    Param _ Nothing -> error "FIXME type inference"
    Param param (Just p_t) -> do
        t <- local_scope_param param p_t (checkm_stmts m_t stmts)
        insert_global (Bound i (SoucFn p_t t))
        pure (Bound i (SoucFn p_t t))


add_top_level_sub :: Identifier -> Maybe Param -> Maybe SoucType -> Stmts
                    -> TopChecker Bound
add_top_level_sub i m_p m_t stmts = case (i, m_t) of
    ("main", _) -> error "tried to add \"main\" as a subroutine"
    (_, Nothing) -> ok_sub
    (_, Just SoucIO) -> ok_sub
    (_, Just wrong) -> error (toString (showb wrong))
    where
        ok_sub = case m_p of
            Nothing -> do
                local_scope (check_stmts SoucIO stmts)
                insert_global (Bound i SoucIO)
                pure (Bound i SoucIO)
            Just (Param _ Nothing) -> error "FIXME type inference"
            Just (Param param (Just p_t)) -> do
                local_scope_param param p_t (check_stmts SoucIO stmts)
                insert_global (Bound i (SoucRoutn p_t))
                pure (Bound i (SoucRoutn p_t))


add_main_routine :: MainParam -> Maybe SoucType -> Stmts -> TopChecker Bound
-- FIXME
-- main can have many different types,
-- depending on which parameter is used
add_main_routine param _m_t stmts = do
    -- FIXME
    -- if a type annotation is given,
    -- it should be checked with the type
    -- that we know the main param must have
    local_scope_main param (check_stmts SoucIO stmts)
    pure $ Bound "main" (SoucRoutn (SoucType "OutputStream" (SoucKind 0)))
