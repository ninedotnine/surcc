{-# LANGUAGE PatternSynonyms #-}

module TypeChecker.Operators (
    in_t,
    ret_t,
    InputType,
    ReturnType,
    pattern Arg,
    pattern Ret,
    infer_prefix_op,
    infer_infix_op
    ) where

import Prelude hiding (lookup)
import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context

import Debug.Trace

newtype InputType  = InputType TypeName
newtype ReturnType = ReturnType TypeName

in_t :: String -> InputType
in_t = InputType . TypeName

ret_t :: String -> ReturnType
ret_t = ReturnType . TypeName

-- pattern Arg :: String -> InputType
-- pattern Arg x <- InputType (TypeName x)
pattern Arg :: TypeName -> InputType
pattern Arg x <- InputType x

pattern Ret :: TypeName -> ReturnType
pattern Ret x <- ReturnType x

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
