{-# LANGUAGE PatternSynonyms #-}

module TypeChecker.Operators (
    in_t,
    ret_t,
    InputType(..),
    ReturnType(..),
    pattern Arg,
    pattern Ret,
    ret,
    ) where

import Prelude hiding (lookup)
import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context

import Debug.Trace

newtype InputType  = InputType SoucType
newtype ReturnType = ReturnType SoucType

in_t :: String -> InputType
in_t = InputType . SoucType . TypeName

ret_t :: String -> ReturnType
ret_t = ReturnType . SoucType . TypeName

ret :: (a, ReturnType) -> SoucType
ret (_, ReturnType x) = x

-- pattern Arg :: String -> InputType
-- pattern Arg x <- InputType (SoucType x)
pattern Arg :: SoucType -> InputType
pattern Arg x <- InputType x

pattern Ret :: SoucType -> ReturnType
pattern Ret x <- ReturnType x
