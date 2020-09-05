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

infer :: Context -> ASTree -> TypeName
infer ctx tree = case tree of
    Branch op left right -> t where (Arg _, Arg _, Ret t) = infer_infix_op op left right
    Twig op expr -> t where (Arg _, Ret t) = infer_prefix_op op expr
    Signed expr _ -> infer ctx expr
    Leaf term -> infer_term ctx term

infer_term :: Context -> Term -> TypeName
infer_term context term = case term of
    LitInt _    -> (TypeName "Integer")
    LitChar _   -> (TypeName "Char")
    LitBool _   -> (TypeName "Bool")
    LitString _ -> (TypeName "String")
    Var v -> case lookup context v of
        Nothing -> TypeName "FIXME OhNOOO"
        Just t -> t

infer_prefix_op :: PrefixOperator -> ASTree -> (InputType, ReturnType)
infer_prefix_op op _ = case op of
    Deref -> error "FIXME"
    GetAddr -> error "FIXME"
    Negate -> (in_t "Bool", ret_t "Bool")
    ToString -> (in_t "Integer", ret_t "String")
    Pure -> error "FIXME"
    Join -> error "FIXME"


infer_infix_op :: Operator -> ASTree -> ASTree -> (InputType, InputType, ReturnType)
infer_infix_op op _ _ = case op of
    Plus  -> (in_t "Integer", in_t "Integer", ret_t "Integer")
    Minus -> (in_t "Integer", in_t "Integer", ret_t "Integer")
    Splat -> (in_t "Integer", in_t "Integer", ret_t "Integer")
    And -> (in_t "Bool", in_t "Bool", ret_t "Bool")
    Or  -> (in_t "Bool", in_t "Bool", ret_t "Bool")
    _ -> error "FIXME"

check_equals :: TypeName -> TypeName -> Maybe TypeError
check_equals t0 t1 = if t0 == t1 then Nothing else Just (TypeError t0 t1)

check_astree :: Context -> ASTree -> TypeName -> Maybe TypeError
check_astree ctx tree t = case tree of
    Branch op left right -> check_astree ctx left l_t
                        <|> check_astree ctx right r_t
                        <|> check_equals t expr_t
        where (Arg l_t, Arg r_t, Ret expr_t) = infer_infix_op op left right

    Twig op expr -> check_astree ctx expr arg_t
                <|> check_equals t expr_t
        where (Arg arg_t, Ret expr_t) = infer_prefix_op op expr

    Leaf term -> check_equals t term_t
        where term_t = infer_term ctx term

    Signed expr sig ->  check_astree ctx expr expr_t
                    <|> check_equals sig expr_t
                    <|> check_equals t expr_t
        where expr_t = infer ctx expr

