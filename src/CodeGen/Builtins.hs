module CodeGen.Builtins (
    gen_builtin_identifier,
    gen_builtin_subroutine,
    gen_builtin_function,
    gen_builtin_constant,
    gen_builtin_data
) where

import Common
import Parser.ExprParser

import qualified Data.HashMap.Strict as Map

type Mapping = Map.HashMap Identifier (String, SoucType)

data BuiltinFunction = BuiltinFunction Identifier String SoucType

data BuiltinSubroutine = BuiltinSubroutine Identifier String SoucType

data BuiltinConstant = BuiltinConstant Identifier String SoucType

data BuiltinData = BuiltinData Identifier String SoucType

gen_builtin_identifier :: String -> Maybe String
gen_builtin_identifier name = Just ("_souc_" ++ name)

gen_builtin_subroutine :: Identifier -> Maybe ASTree -> Maybe String
gen_builtin_subroutine _ _ = Nothing


gen_builtin_function :: Identifier -> ASTree -> Maybe String
gen_builtin_function _ _ = Nothing

gen_builtin_constant :: Identifier -> Maybe String
-- gen_builtin_constant i = fst <$> Map.lookup i builtin_constants
gen_builtin_constant = error "fix pi haha"

gen_builtin_data :: String -> Maybe String
gen_builtin_data s = fst <$> Map.lookup s builtin_data

builtin_subroutines :: Mapping
builtin_subroutines = Map.fromList [
    ("puts", ("_souc_puts", (SoucRoutn (Just (SoucType "String")))))
    ,
    ("write", ("_souc_write", (SoucRoutn (Just (SoucType "String")))))
    ,
    ("abort", ("abort", SoucRoutn Nothing))
    ]

builtin_functions :: Mapping
builtin_functions = Map.fromList [
    ("increment", ("_souc_increment", SoucFn (SoucType "Integer") (SoucType "Integer")))
    ]

builtin_constants :: Mapping
builtin_constants = Map.fromList [
    ("pi", ("3", SoucType "Integer")) -- biblical value
    ]

builtin_data :: Map.HashMap String (String, SoucType)
builtin_data = Map.fromList [
    ("True", ("-1", SoucType "Bool"))
    ,
    ("False", ("0", SoucType "Bool"))
    ,
    ("None", ("_souc_none", SoucMaybe (SoucType "Integer")))
    ]
