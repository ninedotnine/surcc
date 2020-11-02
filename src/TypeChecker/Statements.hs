{-# OPTIONS_GHC -Wno-unused-matches  #-}


module TypeChecker.Statements (
    check_stmts,
    infer_stmts,
) where

import Control.Monad.State
import Data.Either (lefts)
import Prelude hiding (lookup)

import Common
import Parser.Expr.ExprTypes

import TypeChecker.Context
import TypeChecker.Operators
import TypeChecker.Expressions

import Debug.Trace

check_stmts :: Stmts -> Maybe SoucType -> Checker ()
check_stmts (Stmts stmts) m_t = first_left <$> results
    where
        check s = check_stmt s m_t
        results :: State LocalScope [Either TypeError ()]
        results = traverse check stmts

check_stmt :: Stmt -> Maybe SoucType -> Checker ()
check_stmt stmt m_ret = do
    ctx <- get
    case stmt of
        Stmt_While expr body -> check_stmt_while expr body m_ret
        Stmt_Until expr body -> check_stmt_while expr body m_ret
        Stmt_If     expr body m_else -> check_stmt_if expr body m_else m_ret
        Stmt_Unless expr body m_else -> check_stmt_if expr body m_else m_ret
        Stmt_Sub_Call name m_arg -> check_stmt_call name m_arg
        Stmt_Postfix_Oper name oper -> pure (Right ()) -- FIXME
        Stmt_Const_Assign name m_t expr -> check_stmt_ass name (SoucType <$> m_t) (BindMayExist False) expr
        Stmt_Var_Assign name m_t expr -> check_stmt_ass name (SoucType <$> m_t) (BindMayExist True) expr
        Stmt_Return m_expr -> check_stmt_return m_expr m_ret

first_left :: [Either TypeError ()] -> Either TypeError ()
first_left list = case lefts list of
    [] -> Right ()
    (l:_) -> Left l

soucbool :: SoucType
soucbool = SoucType (TypeName "Bool")

check_stmt_return :: Maybe ASTree -> Maybe SoucType -> Checker ()
check_stmt_return m_expr m_ret = do
    ctx <- get
    case m_expr of
        Just expr -> case m_ret of
            Just t -> case check_astree ctx expr t of
                Left err -> pure (Left err)
                Right () -> pure (Right ())
            Nothing -> case infer ctx expr of
                Left err -> pure (Left err)
                Right t -> pure $ Left (TypeMismatch (SoucType "IO") t)
        Nothing -> case m_ret of
            Just t -> pure $ Left (TypeMismatch t (SoucType "IO"))
            Nothing -> pure (Right ())


check_stmt_while :: ASTree -> Stmts -> Maybe SoucType -> Checker ()
check_stmt_while expr body m_ret = do
    ctx <- get
    case check_astree ctx expr soucbool of
        Left err -> pure (Left err)
        Right () -> check_stmts body m_ret

check_stmt_if :: ASTree -> Stmts -> (Maybe Stmts) -> (Maybe SoucType) -> Checker ()
check_stmt_if expr body m_else m_ret = do
    ctx <- get
    case check_astree ctx expr soucbool of
        Left err -> pure (Left err)
        Right () -> do
            stmts <- check_stmts body m_ret
            case stmts of
                Left err -> pure (Left err)
                Right () -> case m_else of
                    Nothing -> pure (Right ())
                    Just else_body -> check_stmts else_body m_ret

check_stmt_ass :: Identifier -> (Maybe SoucType) -> BindMayExist -> ASTree -> Checker ()
check_stmt_ass name m_t may_exist expr = do
    ctx <- get
    case m_t of
        Nothing -> case infer ctx expr of
            Left err -> pure (Left err)
            Right t -> insert may_exist (Bound name t)
        Just t -> case check_astree ctx expr t of
            Left err -> pure (Left err)
            Right () -> insert may_exist (Bound name t)

check_stmt_call :: Identifier -> Maybe ASTree -> Checker ()
check_stmt_call name m_expr = do
    ctx <- get
    case (lookup ctx name , m_expr) of
        (Just (SoucType "IO"), Nothing) -> pure (Right ())
        (Just (SoucRoutn t), Just expr) -> case check_astree ctx expr t of
            Left err -> pure (Left err)
            Right () -> pure (Right ())
        (Just t, _) -> pure $ Left $ TypeMismatch (SoucType "IO") t
        (Nothing, _) -> pure $ Left $ Undeclared name

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
    Stmt_Return m_expr -> undefined
