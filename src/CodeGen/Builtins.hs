module CodeGen.Builtins (
    gen_builtin_identifier,
    gen_builtin_subroutine,
    gen_builtin_function,
    gen_builtin_constant,
    gen_builtin_data
) where

import Common (
    Stmt(..),
    Param(..),
    Identifier(..),
    )
import Parser.ExprParser

data BuiltinFunction = BuiltinFunction Identifier String

data BuiltinSubroutine = BuiltinSubroutine Identifier String

data BuiltinConstant = BuiltinConstant Identifier String

gen_builtin_identifier :: String -> Maybe String
gen_builtin_identifier name = Just ("_souc_" ++ name)

gen_builtin_subroutine :: Identifier -> Maybe ASTree -> Maybe String
gen_builtin_subroutine _ _ = Nothing


gen_builtin_function :: Identifier -> ASTree -> Maybe String
gen_builtin_function _ _ = Nothing

gen_builtin_constant :: Identifier -> Maybe String
gen_builtin_constant = undefined

gen_builtin_data :: String -> Maybe String
gen_builtin_data s = case s of
    "True" -> Just "-1"
    "False" -> Just "0"
    "None" -> Just "_souc_none"
    _ -> Nothing

builtin_subroutines = [
    BuiltinSubroutine "puts" "_souc_puts",
    BuiltinSubroutine "write" "_souc_write"
    ,
    BuiltinSubroutine "abort" "abort"
    ]

builtin_functions = [
    BuiltinFunction "increment" "_souc_increment"
    ]

builtin_constants = [
    BuiltinConstant "pi" "3"  -- biblical value
    ]
