module TypeChecker.Expressions (
    infer,
    check_astree
) where

import Control.Applicative

import Prelude hiding (lookup)
import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context
import TypeChecker.Operators

infer :: Context -> ASTree -> Either TypeError TypeName
infer ctx tree = case tree of
    Branch op left right -> ret <$> infer_infix_op op left right
    Twig op expr -> ret <$> infer_prefix_op op expr
    Signed expr t -> do
        inferred <- infer ctx expr
        check_equals t inferred
        Right inferred
    Leaf term -> infer_term ctx term

infer_term :: Context -> Term -> Either TypeError TypeName
infer_term context term = case term of
    LitInt _    -> Right (TypeName "Integer")
    LitChar _   -> Right (TypeName "Char")
    LitBool _   -> Right (TypeName "Bool")
    LitString _ -> Right (TypeName "String")
    Var v -> case lookup context v of
        Nothing -> Left (Undeclared v)
        Just t -> Right t

not_implemented :: Either TypeError a
not_implemented = Left $ TypeMismatch (TypeName "NOT YET") (TypeName "IMPLEMENTED")

infer_prefix_op :: PrefixOperator -> ASTree -> Either TypeError (InputType, ReturnType)
infer_prefix_op op _ = case op of
    Deref -> not_implemented
    GetAddr -> not_implemented
    Negate -> Right (in_t "Bool", ret_t "Bool")
    ToString -> Right (in_t "Integer", ret_t "String")
    Pure -> not_implemented
    Join -> not_implemented


infer_infix_op :: Operator -> ASTree -> ASTree -> Either TypeError ((InputType, InputType), ReturnType)
infer_infix_op op _ _ = case op of
    Plus  -> Right ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    Minus -> Right ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    Splat -> Right ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    And -> Right ((in_t "Bool", in_t "Bool"), ret_t "Bool")
    Or  -> Right ((in_t "Bool", in_t "Bool"), ret_t "Bool")
    _ -> not_implemented

check_equals :: TypeName -> TypeName -> Either TypeError ()
check_equals t0 t1 = if t0 == t1 then Right () else Left (TypeMismatch t0 t1)

check_astree :: Context -> ASTree -> TypeName -> Either TypeError ()
check_astree ctx tree t = case tree of
    Branch op left right -> case infer_infix_op op left right of
        Left err -> Left err
        Right ((InputType l_t, InputType r_t), ReturnType expr_t) -> do
            check_astree ctx left l_t
            check_astree ctx right r_t
            check_equals t expr_t

    Twig op expr -> case infer_prefix_op op expr of
        Left err -> Left err
        Right (InputType arg_t, ReturnType expr_t) -> do
            check_astree ctx expr arg_t
            check_equals t expr_t

    Leaf term -> do
        term_t <- infer_term ctx term
        check_equals t term_t

    Signed expr sig -> do
        expr_t <- infer ctx expr
        check_astree ctx expr expr_t
        check_equals sig expr_t
        check_equals t expr_t
