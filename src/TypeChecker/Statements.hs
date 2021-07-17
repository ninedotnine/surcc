{-# OPTIONS_GHC -Wno-unused-matches  #-}


module TypeChecker.Statements (
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

import Common
import Parser.Expr.ExprTypes

import TypeChecker.Context
import TypeChecker.Operators
import TypeChecker.Expressions

import Debug.Trace

check_stmts :: Stmts -> Maybe SoucType -> Checker ()
check_stmts (Stmts stmts) m_t = do
    mapM_ check stmts
    where
        check s = check_stmt s m_t

check_stmt :: Stmt -> Maybe SoucType -> Checker ()
check_stmt stmt m_ret = do
    ctx <- get
    case stmt of
        Stmt_While expr body -> check_stmt_while expr body m_ret
        Stmt_Until expr body -> check_stmt_while expr body m_ret
        Stmt_If     expr body m_else -> check_stmt_if expr body m_else m_ret
        Stmt_Unless expr body m_else -> check_stmt_if expr body m_else m_ret
        Stmt_Sub_Call name m_arg -> check_stmt_call name m_arg
        Stmt_Postfix_Oper name oper -> pure () -- FIXME
        Stmt_Const_Assign name m_t expr -> check_stmt_ass name m_t expr
        Stmt_Var_Assign name m_t expr -> check_stmt_mut_ass name m_t expr
        Stmt_Var_Reassign name expr -> check_stmt_mut_ass name Nothing expr
        Stmt_Return m_expr -> check_stmt_return m_expr m_ret

soucbool :: SoucType
soucbool = SoucType "Bool"

check_stmt_return :: Maybe ASTree -> Maybe SoucType -> Checker ()
check_stmt_return m_expr m_ret = do
    ctx <- get
    case m_expr of
        Just expr -> case m_ret of
            Just t -> case check_astree ctx expr t of
                Left err -> throwE err
                Right () -> pure ()
            Nothing -> case infer ctx expr of
                Left err -> throwE err
                Right t -> throwE (TypeMismatch SoucIO t)
        Nothing -> case m_ret of
            Just t -> throwE (TypeMismatch t SoucIO)
            Nothing -> pure ()


check_stmt_while :: ASTree -> Stmts -> Maybe SoucType -> Checker ()
check_stmt_while expr body m_ret = do
    ctx <- get
    case check_astree ctx expr soucbool of
        Left err -> throwE err
        Right () -> check_stmts body m_ret

check_stmt_if :: ASTree -> Stmts -> (Maybe Stmts) -> (Maybe SoucType) -> Checker ()
check_stmt_if expr body m_else m_ret = do
    ctx <- get
    case check_astree ctx expr soucbool of
        Left err -> throwE err
        Right () -> do
            check_stmts body m_ret
            case m_else of
                Nothing -> pure ()
                Just else_body -> check_stmts else_body m_ret

check_stmt_ass :: Identifier -> (Maybe SoucType) -> ASTree -> Checker ()
check_stmt_ass name m_t expr = do
    t <- infer_if_needed m_t expr
    insert_const (Bound name t)

check_stmt_mut_ass :: Identifier -> (Maybe SoucType) -> ASTree -> Checker ()
check_stmt_mut_ass name m_t expr = do
    t <- infer_if_needed m_t expr
    insert_mut (Bound name t)


check_stmt_call :: Identifier -> Maybe ASTree -> Checker ()
check_stmt_call name m_expr = do
    ctx <- get
    case (lookup ctx name , m_expr) of
        (Just SoucIO, Nothing) -> pure ()
        (Just (SoucRoutn param), Just expr) -> case check_astree ctx expr param of
            Left err -> throwE err
            Right () -> pure ()
        (Just t, _) -> throwE $ TypeMismatch SoucIO t
        (Nothing, _) -> throwE $ Undeclared name

infer_stmts :: LocalScope -> Stmts -> Either TypeError SoucType
infer_stmts ctx (Stmts stmts) = undefined

infer_stmt :: LocalScope -> Stmt -> Either TypeError SoucType
infer_stmt ctx stmt = case stmt of
    Stmt_While expr body -> undefined
    Stmt_Until expr body -> undefined
    Stmt_If expr body m_else -> undefined
    Stmt_Unless expr body m_else -> undefined
    Stmt_Sub_Call name m_arg -> undefined
    Stmt_Postfix_Oper name oper -> undefined
    Stmt_Const_Assign name m_t expr -> undefined
    Stmt_Var_Assign name m_t expr -> undefined
    Stmt_Var_Reassign name expr -> undefined
    Stmt_Return m_expr -> undefined
