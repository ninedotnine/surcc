module CodeGen.Builtins (
    gen_builtin_identifier,
    gen_builtin_subroutine,
    gen_builtin_function,
    gen_builtin_constant
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


gen_builtin_function :: Identifier -> Maybe ASTree -> Maybe String
gen_builtin_function _ _ = Nothing

gen_builtin_constant :: Identifier -> Maybe ASTree -> Maybe String
gen_builtin_constant = undefined

builtin_subroutines = [
    BuiltinSubroutine "writey" "_souc_writey"
    ,
    BuiltinSubroutine "abort" "abort"
    ]

writey :: ASTree -> String
writey expr = case expr of
    Branch Tuple (Leaf _) (Leaf (LitString msg)) -> msg
    _ -> error "if you reached here, then there was an error in type-checking"

builtin_functions = [
    BuiltinFunction "increment" "_souc_increment"
    ]

builtin_constants = [
    BuiltinConstant "pi" "3"  -- biblical value
    ]
