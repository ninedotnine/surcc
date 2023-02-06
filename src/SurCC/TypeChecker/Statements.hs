{-# OPTIONS_GHC -Wno-unused-matches  #-}

module SurCC.TypeChecker.Statements (
    check_stmts,
    infer_stmts,
) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Except
import Data.Either (lefts)
import Data.Traversable
import Prelude hiding (lookup)

import SurCC.Common

import SurCC.TypeChecker.Context (Checker,
                            LocalScope,
                            insert_immut,
                            insert_mut,
                            lookup)
import SurCC.TypeChecker.Expressions (
    check_expr,
    infer,
    infer_if_needed,
    )


check_stmts :: Stmts -> Maybe SoucType -> Checker ()
check_stmts (Stmts stmts m_ret) m_t = do
    mapM_ check stmts
    case m_ret of
        Just (Return expr) -> check_return expr m_t
        Nothing -> pure ()
    where
        check s = check_stmt s m_t

check_stmt :: Stmt -> Maybe SoucType -> Checker ()
check_stmt stmt m_ret = do
    case stmt of
        Stmt_While expr body -> check_stmt_while expr body m_ret
        Stmt_Until expr body -> check_stmt_while expr body m_ret
        Stmt_If     expr body m_else -> check_stmt_if expr body m_else m_ret
        Stmt_Unless expr body m_else -> check_stmt_if expr body m_else m_ret
        Stmt_Sub_Call name m_arg -> check_stmt_call name m_arg
        Stmt_Postfix_Oper name oper -> pure () -- FIXME delete this?
        Stmt_Const_Assign_Static name m_t expr ->
            check_stmt_ass name m_t expr
        Stmt_Const_Assign_Dynamic name m_t expr ->
            check_stmt_ass name m_t expr
        -- FIXME
        -- the typechecker relies on the parser to not allow
        -- redeclaring of the same variable.
        -- eventually this functionality should be removed from the parser
        Stmt_Var_Declare name m_t expr -> check_stmt_mut_ass name m_t expr
        Stmt_Var_Reassign name m_t expr -> check_stmt_mut_ass name m_t expr
    where
        check_stmt_ass :: Identifier -> (Maybe SoucType) -> ExprTree -> Checker ()
        check_stmt_ass name m_t expr = do
            t <- infer_if_needed m_t expr
            insert_immut name t

        check_stmt_mut_ass :: Identifier -> (Maybe SoucType) -> ExprTree -> Checker ()
        check_stmt_mut_ass name m_t expr = do
            t <- infer_if_needed m_t expr
            insert_mut name t


check_return :: Maybe ExprTree -> Maybe SoucType -> Checker ()
check_return m_expr m_ret = do
    case m_expr of
        Just expr -> case m_ret of
            Just t -> check_expr t expr
            Nothing -> do
                expr_t <- infer expr
                throwE (TypeMismatch SoucIO expr_t)
        Nothing -> case m_ret of
            Just t -> throwE (TypeMismatch t SoucIO)
            Nothing -> pure ()


check_stmt_while :: ExprTree -> Stmts -> Maybe SoucType -> Checker ()
check_stmt_while expr body m_ret = do
    check_expr SoucBool expr
    check_stmts body m_ret


check_stmt_if :: ExprTree -> Stmts -> (Maybe Stmts) -> (Maybe SoucType) -> Checker ()
check_stmt_if expr body m_else m_ret = do
    check_expr SoucBool expr
    check_stmts body m_ret
    case m_else of
        Nothing -> pure ()
        Just else_body -> check_stmts else_body m_ret


check_stmt_call :: Identifier -> Maybe ExprTree -> Checker ()
check_stmt_call name m_expr = do
    ctx <- get
    case (lookup name ctx, m_expr) of
        (Just SoucIO, Nothing) -> pure ()
        (Just (SoucRoutn param), Just expr) -> check_expr param expr
        (Just t, _) -> throwE $ TypeMismatch SoucIO t
        (Nothing, _) -> throwE $ Undeclared name

infer_stmts :: Stmts -> Either TypeError SoucType
infer_stmts (Stmts _ m_ret) = case m_ret of
    Just (Return r) -> undefined
    Nothing -> undefined
