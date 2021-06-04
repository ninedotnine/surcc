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
import Data.Text (Text)
import qualified Data.Text as Text

import Common
import Parser.Expr.ExprTypes
import TypeChecker.Context

newtype InputType  = InputType SoucType
newtype ReturnType = ReturnType SoucType

in_t :: Text -> InputType
in_t = InputType . SoucType

ret_t :: Text -> ReturnType
ret_t = ReturnType . SoucType

ret :: (a, ReturnType) -> SoucType
ret (_, ReturnType x) = x

-- FIXME these patterns might be unused.

-- pattern Arg :: String -> InputType
-- pattern Arg x <- InputType (SoucType x)
pattern Arg :: SoucType -> InputType
pattern Arg x <- InputType x

pattern Ret :: SoucType -> ReturnType
pattern Ret x <- ReturnType x
