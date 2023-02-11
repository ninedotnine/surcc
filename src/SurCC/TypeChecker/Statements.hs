{-# OPTIONS_GHC -Wno-unused-matches  #-}

module SurCC.TypeChecker.Statements (
    check_stmts,
    infer_stmts,
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Foldable (for_)
import Data.Either (lefts)
import Data.Traversable
import Prelude hiding (lookup)

import SurCC.Common

import SurCC.TypeChecker.Context (Checker,
                            LocalScope,
                            insert_local,
                            get_type,
                            lookup_scopes_mutables)
import SurCC.TypeChecker.Expressions (
    check_expr,
    infer,
    infer_if_needed,
    assert_equals,
    )


check_stmts :: SoucType -> Stmts -> Checker ()
check_stmts t (Stmts stmts m_ret) = do
    mapM_ (check_stmt t) stmts
    for_ m_ret (check_return t)

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
            expr_t <- infer_if_needed m_t expr
            insert_local Immut name expr_t

        check_stmt_mut_ass :: Identifier -> (Maybe SoucType) -> ExprTree
                              -> Checker ()
        check_stmt_mut_ass name m_t expr = do
            expr_t <- infer_if_needed m_t expr
            insert_local Mut name expr_t


infer_return :: Maybe ExprTree -> Checker SoucType
infer_return m_expr = case m_expr of
    Just expr -> do
        expr_t <- infer expr
        throwE (TypeMismatch SoucIO expr_t)
    Nothing -> pure SoucIO


check_return :: SoucType -> Return -> Checker ()
check_return t = \case
    Return (Just expr) -> check_expr t expr
    Return Nothing -> assert_equals SoucIO t


check_stmt_reassign :: Maybe SoucType -> Identifier -> ExprTree -> Checker ()
check_stmt_reassign m_t name expr = do
    expr_t <- infer_if_needed m_t expr
    ctx <- get
    case lookup_scopes_mutables name ctx of
        Nothing -> throwE (Undeclared name)
        Just (tp, mut) -> do
            assert_equals tp expr_t
            when (mut /= Mut) $
                throwE (BadReassign name)


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
                           >>= throwE . TypeMismatch SoucIO
        (t, _) -> throwE $ TypeMismatch SoucIO t


infer_stmts :: Stmts -> Checker SoucType
infer_stmts (Stmts _ t) = case t of
    Just (Return r) -> undefined
    Nothing -> undefined


infer_or_check_stmts :: Maybe SoucType -> Stmts -> Checker SoucType
infer_or_check_stmts m_t stmts = do
    case m_t of
        Nothing -> infer_stmts stmts
        Just t -> check_stmts t stmts *> pure t
