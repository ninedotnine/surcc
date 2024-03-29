{-# OPTIONS_GHC -Wno-unused-matches  #-}

module SurCC.TypeChecker.Statements (
    check_stmts,
    checkm_stmts,
    infer_stmts,
) where

import Control.Monad
import Control.Monad.Error.Class (throwError)
import Data.Foldable (for_)
import Data.Either (lefts)
import Data.Traversable (for)
import Prelude hiding (lookup)

import SurCC.Common
import SurCC.Common.SoucTypes

import SurCC.TypeChecker.Context (
    Checker,
    insert_local,
    get_type,
    get_var,
    new_scope,
    exit_scope,
    assert_type_exists,
    add_type_vars,
    )
import SurCC.TypeChecker.Expressions (
    check_expr,
    infer,
    checkm_expr,
    assert_equals,
    )


check_stmts :: SoucType -> Stmts -> Checker ()
check_stmts t (Stmts stmts m_ret) = do
    new_scope
    mapM_ (check_stmt t) stmts
    for_ m_ret (check_return t)
    exit_scope


check_stmt :: SoucType -> Stmt -> Checker ()
check_stmt t = \case
    Stmt_While expr body -> check_stmt_while t expr body
    Stmt_Until expr body -> check_stmt_while t expr body
    Stmt_If     expr body m_else -> check_stmt_if t expr body m_else
    Stmt_Unless expr body m_else -> check_stmt_if t expr body m_else
    Stmt_Sub_Call name m_arg -> check_stmt_call t name m_arg
    Stmt_Postfix_Oper name oper -> pure () -- FIXME delete this?
    Stmt_Const_Assign_Static name m_t expr -> check_stmt_ass name m_t expr
    Stmt_Const_Assign_Dynamic name m_t expr -> check_stmt_ass name m_t expr
    -- FIXME
    -- the typechecker relies on the parser to not allow
    -- redeclaring of the same variable.
    -- eventually this functionality should be removed from the parser
    Stmt_Var_Declare name m_t expr -> check_stmt_mut_ass name m_t expr
    Stmt_Var_Reassign name m_t expr -> check_stmt_reassign m_t name expr
    where
        check_stmt_ass :: Identifier -> (Maybe SoucType) -> ExprTree
                          -> Checker ()
        check_stmt_ass name m_t expr = do
            expr_t <- checkm_expr m_t expr
            insert_local Immut name expr_t

        check_stmt_mut_ass :: Identifier -> (Maybe SoucType) -> ExprTree
                              -> Checker ()
        check_stmt_mut_ass name m_t expr = do
            expr_t <- checkm_expr m_t expr
            insert_local Mut name expr_t


infer_return :: Return -> Checker SoucType
infer_return = \case
    Return (Just expr) -> infer expr
                    -- FIXME why throw an error?
                    -- the actual FIXME is that `infer_stmts` is not even used
                 >>= throwError . TypeMismatch SoucIO
    Return Nothing -> pure SoucIO


check_return :: SoucType -> Return -> Checker ()
check_return t = \case
    Return (Just expr) -> check_expr t expr
    Return Nothing -> assert_equals SoucIO t


check_stmt_reassign :: Maybe SoucType -> Identifier -> ExprTree -> Checker ()
check_stmt_reassign m_t name expr = do
    expr_t <- checkm_expr m_t expr
    t <- get_var name
    assert_equals t expr_t


check_stmt_while :: SoucType -> ExprTree -> Stmts -> Checker ()
check_stmt_while t expr body = do
    check_expr SoucBool expr
    check_stmts t body


check_stmt_if :: SoucType -> ExprTree -> Stmts -> (Maybe Stmts) -> Checker ()
check_stmt_if t expr body m_else = do
    check_expr SoucBool expr
    check_stmts t body
    for_ m_else (check_stmts t)


check_stmt_call :: SoucType -> Identifier -> Maybe ExprTree -> Checker ()
check_stmt_call _t name m_expr = do
    m_type <- get_type name
    case (m_type, m_expr) of
        (SoucIO, Nothing) -> pure ()
        (SoucRoutn param, Just expr) -> check_expr param expr
        (SoucIO, Just expr) -> infer expr
                               >>= throwError . TypeMismatch SoucIO
        (t, _) -> throwError $ TypeMismatch SoucIO t


infer_stmts :: Stmts -> Checker SoucType
infer_stmts (Stmts stmts m_ret) = do
    new_scope
    l_m_t0 <- mapM infer_stmt stmts
    m_t0 <- foldM assert_maybe_equals Nothing l_m_t0
    m_t1 <- for m_ret infer_return
    exit_scope
    assert_maybe_equals m_t0 m_t1 >>= \case
        Just t -> pure t
        Nothing -> undefined


-- a Stmt could contain a Stmts, which could contain a Return
infer_stmt :: Stmt -> Checker (Maybe SoucType)
infer_stmt = undefined


checkm_stmts :: Maybe SoucType -> Stmts -> Checker SoucType
checkm_stmts m_t stmts = case m_t of
    Nothing -> infer_stmts stmts
    Just t -> do
        t_added <- add_type_vars t
        check_stmts t_added stmts
        pure t_added


assert_maybe_equals :: Maybe SoucType -> Maybe SoucType -> Checker (Maybe SoucType)
assert_maybe_equals m_t0 m_t1 = case (m_t0,m_t1) of
    (Just t0,Just t1) -> assert_equals t0 t1 *> pure (Just t0)
    (Just t0,Nothing) -> pure $ Just t0
    (Nothing,Just t1) -> pure $ Just t1
    (Nothing,Nothing) -> pure Nothing
