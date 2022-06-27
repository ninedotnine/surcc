module SurCC.TypeChecker.Expressions (
    infer,
    check_expr,
    infer_if_needed,
) where

import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.State

import SurCC.Common
import SurCC.TypeChecker.Context (lookup_identifier, LocalScope, Checker)
import SurCC.TypeChecker.Operators

infer :: LocalScope -> ExprTree -> Either TypeError SoucType
infer ctx tree = case tree of
    Branch op left right -> ret <$> infer_infix_op ctx op left right
    Twig op expr -> ret <$> infer_prefix_op op expr
    Signed expr t -> do
        inferred <- infer ctx expr
        check_equals t inferred
        Right inferred
    Leaf term -> infer_term ctx term

infer_term :: LocalScope -> Term -> Either TypeError SoucType
infer_term context term = case term of
    LitInt _    -> Right SoucInteger
    LitChar _   -> Right SoucChar
    LitString _ -> Right SoucString
    Var v -> case lookup_identifier v context of
        Nothing -> Left (Undeclared v)
        Just t -> Right t
    Constructor s -> case s of
        "True" -> Right SoucBool
        "False" -> Right SoucBool
        "None" -> Right (SoucMaybe SoucInteger)
        "OK" -> Right (SoucMaybe (SoucFn SoucInteger SoucInteger))
        _ -> Left (UnknownData s)


not_implemented :: Either TypeError a
not_implemented = Left $ TypeMismatch (SoucType "NOT YET" (SoucKind 0)) (SoucType "IMPLEMENTED" (SoucKind 0))

infer_prefix_op :: PrefixOperator -> ExprTree -> Either TypeError (InputType, ReturnType)
infer_prefix_op op _ = case op of
    Deref -> not_implemented
    GetAddr -> not_implemented
    Negate -> Right (in_t "Bool", ret_t "Bool")
    ToString -> Right (in_t "Integer", ret_t "String")
    Pure -> not_implemented
    Join -> not_implemented


infer_infix_op :: LocalScope -> Operator -> ExprTree -> ExprTree
                  -> Either TypeError ((InputType, InputType), ReturnType)
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
            _ -> Left (TypeMismatch (SoucFn l_t (SoucTypeVar (TypeVar (Right 'T') (SoucKind 0)))) l_t)
    FlipApply -> do
        r_t <- infer ctx right
        l_t <- infer ctx left
        case r_t of
            SoucFn t0 t1 -> do
                check_equals l_t t0
                Right (((InputType t0, InputType r_t)), ReturnType t1)
            _ -> Left (TypeMismatch (SoucFn l_t (SoucTypeVar (TypeVar (Right 'T') (SoucKind 0)))) r_t)
    Comma -> do
        l_t <- infer ctx left
        r_t <- infer ctx right
        Right ((InputType l_t, InputType r_t), ReturnType (SoucPair l_t r_t))

    _ -> not_implemented

check_equals :: SoucType -> SoucType -> Either TypeError ()
check_equals t0 t1 = if t0 == t1 then Right () else Left (TypeMismatch t0 t1)

check_expr :: LocalScope -> ExprTree -> SoucType -> Either TypeError ()
check_expr ctx tree t = case tree of
    Branch op left right -> do
        ((InputType l_t, InputType r_t), ReturnType expr_t) <- infer_infix_op ctx op left right
        check_expr ctx left l_t
        check_expr ctx right r_t
        check_equals t expr_t


    Twig op expr -> do
        (InputType arg_t, ReturnType expr_t) <- infer_prefix_op op expr
        check_expr ctx expr arg_t
        check_equals t expr_t

    Leaf term -> do
        term_t <- infer_term ctx term
        check_equals t term_t

    Signed expr sig -> do
        expr_t <- infer ctx expr
        check_expr ctx expr expr_t
        check_equals sig expr_t
        check_equals t expr_t


infer_if_needed :: Maybe SoucType -> ExprTree -> Checker SoucType
infer_if_needed m_t expr = do
    ctx <- get
    case m_t of
        Nothing -> case infer ctx expr of
            Right t -> pure t
            Left err -> throwE err
        Just t -> case check_expr ctx expr t of
            Right () -> pure t
            Left err -> throwE err
