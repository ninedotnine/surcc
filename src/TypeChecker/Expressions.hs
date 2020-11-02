{-# LANGUAGE PatternSynonyms #-}

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

infer :: LocalScope -> ASTree -> Either TypeError SoucType
infer ctx tree = case tree of
    Branch op left right -> ret <$> infer_infix_op ctx op left right
    Twig op expr -> ret <$> infer_prefix_op op expr
    Signed expr t -> do
        inferred <- infer ctx expr
        check_equals (SoucType t) inferred
        Right inferred
    Leaf term -> infer_term ctx term

infer_term :: LocalScope -> Term -> Either TypeError SoucType
infer_term context term = case term of
    LitInt _    -> Right (SoucType "Integer")
    LitChar _   -> Right (SoucType "Char")
    LitBool _   -> Right (SoucType "Bool")
    LitString _ -> Right (SoucType "String")
    Var v -> case lookup context v of
        Nothing -> Left (Undeclared v)
        Just t -> Right t

not_implemented :: Either TypeError a
not_implemented = Left $ TypeMismatch (SoucType "NOT YET") (SoucType "IMPLEMENTED")

infer_prefix_op :: PrefixOperator -> ASTree -> Either TypeError (InputType, ReturnType)
infer_prefix_op op _ = case op of
    Deref -> not_implemented
    GetAddr -> not_implemented
    Negate -> Right (in_t "Bool", ret_t "Bool")
    ToString -> Right (in_t "Integer", ret_t "String")
    Pure -> not_implemented
    Join -> not_implemented


infer_infix_op :: LocalScope -> Operator -> ASTree -> ASTree -> Either TypeError ((InputType, InputType), ReturnType)
infer_infix_op ctx op left right = case op of
    Plus  -> Right ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    Minus -> Right ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    Splat -> Right ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    And -> Right ((in_t "Bool", in_t "Bool"), ret_t "Bool")
    Or  -> Right ((in_t "Bool", in_t "Bool"), ret_t "Bool")
    Equals -> Right ((in_t "Integer", in_t "Integer"), ret_t "Bool") -- FIXME (should be general)
    LesserThan -> Right ((in_t "Integer", in_t "Integer"), ret_t "Bool") -- FIXME (should be general)
    Apply     -> do
        l_t <- infer ctx left
        case l_t of
            SoucFn t0 t1 -> do
                r_t <- infer ctx right
                check_equals r_t t0
                Right ((InputType l_t, InputType t0), ReturnType t1)
            _ -> Left (TypeMismatch (SoucFn l_t (SoucType "T")) l_t)
    FlipApply -> do
        r_t <- infer ctx right
        l_t <- infer ctx left
        case r_t of
            SoucFn t0 t1 -> do
                check_equals l_t t0
                Right (((InputType t0, InputType r_t)), ReturnType t1)
            _ -> Left (TypeMismatch (SoucFn l_t (SoucType "T")) r_t)
    _ -> not_implemented

check_equals :: SoucType -> SoucType -> Either TypeError ()
check_equals t0 t1 = if t0 == t1 then Right () else Left (TypeMismatch t0 t1)

check_astree :: LocalScope -> ASTree -> SoucType -> Either TypeError ()
check_astree ctx tree t = case tree of
    Branch op left right -> do
        ((InputType l_t, InputType r_t), ReturnType expr_t) <- infer_infix_op ctx op left right
        check_astree ctx left l_t
        check_astree ctx right r_t
        check_equals t expr_t


    Twig op expr -> do
        (InputType arg_t, ReturnType expr_t) <- infer_prefix_op op expr
        check_astree ctx expr arg_t
        check_equals t expr_t

    Leaf term -> do
        term_t <- infer_term ctx term
        check_equals t term_t

    Signed expr sig -> do
        expr_t <- infer ctx expr
        check_astree ctx expr expr_t
        check_equals (SoucType sig) expr_t
        check_equals t expr_t
