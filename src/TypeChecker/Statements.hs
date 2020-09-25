{-# LANGUAGE OverloadedStrings #-}

module TypeChecker.Statements (
    check_stmts,
    infer_stmts,
) where

import Prelude hiding (lookup)
import Data.Either (lefts)

import Common
import Parser.Expr.ExprTypes

import TypeChecker.Context
import TypeChecker.Operators
import TypeChecker.Expressions

check_stmts :: Context -> Stmts -> Maybe SoucType -> Either TypeError ()
check_stmts ctx (Stmts stmts) t = first_left (map check stmts)
    where check s = check_stmt ctx s t

first_left :: [Either TypeError ()] -> Either TypeError ()
first_left list = case lefts list of
    [] -> Right ()
    (l:_) -> Left l

soucbool :: SoucType
soucbool = SoucType (TypeName "Bool")

check_stmt :: Context -> Stmt -> Maybe SoucType -> Either TypeError ()
check_stmt ctx stmt m_ret = case stmt of
    Stmt_While expr body -> check_stmt_while ctx expr body m_ret
    Stmt_Until expr body -> check_stmt_while ctx expr body m_ret
    Stmt_If     expr body m_else -> check_stmt_if ctx expr body m_else m_ret
    Stmt_Unless expr body m_else -> check_stmt_if ctx expr body m_else m_ret
    Stmt_Sub_Call name m_arg -> undefined
    Stmt_Postfix_Oper name oper -> undefined
    Stmt_Const_Assign name expr -> undefined
    Stmt_Var_Assign name m_t expr -> check_stmt_ass ctx name (SoucType <$> m_t) expr
    Stmt_Return m_expr -> check_stmt_return ctx m_expr m_ret


check_stmt_return :: Context -> Maybe ASTree -> Maybe SoucType -> Either TypeError ()
check_stmt_return ctx m_expr m_ret = case m_expr of
    Just expr -> case m_ret of
        Just t -> check_astree ctx expr t
        Nothing -> case infer ctx expr of
            Left err -> Left err
            Right t -> Left (TypeMismatch (SoucType "IO") t)
    Nothing -> case m_ret of
        Just t -> Left (TypeMismatch t (SoucType "IO"))
        Nothing -> Right ()

check_stmt_while :: Context -> ASTree -> Stmts -> Maybe SoucType -> Either TypeError ()
check_stmt_while ctx expr body m_ret = do
    check_astree ctx expr soucbool
    check_stmts ctx body m_ret

check_stmt_if :: Context -> ASTree -> Stmts -> (Maybe Stmts) -> (Maybe SoucType) -> Either TypeError ()
check_stmt_if ctx expr body m_else m_ret = do
    check_astree ctx expr soucbool
    check_stmts ctx body m_ret
    case m_else of
        Nothing -> Right ()
        Just else_body -> check_stmts ctx else_body m_ret

-- FIXME needs to add the identifier to the context (if it is not there)
-- all this stuff will need to use the state monad, i guess
check_stmt_ass :: Context -> Identifier -> (Maybe SoucType) -> ASTree -> Either TypeError ()
check_stmt_ass ctx name m_t expr = case m_t of
    Nothing -> infer ctx expr >> pure ()
    Just t -> check_astree ctx expr t

infer_stmts :: Context -> Stmts -> Either TypeError SoucType
infer_stmts ctx (Stmts stmts) = undefined

infer_stmt :: Context -> Stmt -> Either TypeError SoucType
infer_stmt ctx stmt = case stmt of
    Stmt_While expr body -> undefined
    Stmt_Until expr body -> undefined
    Stmt_If expr body m_else -> undefined
    Stmt_Unless expr body m_else -> undefined
    Stmt_Sub_Call name m_arg -> undefined
    Stmt_Postfix_Oper name oper -> undefined
    Stmt_Const_Assign name expr -> undefined
    Stmt_Var_Assign name m_t expr -> undefined
    Stmt_Return m_expr -> undefined
