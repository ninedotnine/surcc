{-# LANGUAGE PatternSynonyms #-}

module TypeChecker.Operators (
    in_t,
    ret_t,
    InputType,
    ReturnType,
    pattern Arg,
    pattern Ret,
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

