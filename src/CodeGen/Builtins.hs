module CodeGen.Builtins (
    gen_builtin_identifier,
    gen_builtin_data,
    typeof_builtin_identifier,
    typeof_builtin_data,
    BuiltinsCtx(..),
) where

import Common
import Parser.ExprParser

import qualified Data.HashMap.Strict as Map

type Mapping = Map.HashMap String (String, SoucType)

newtype BuiltinsCtx = Builtins [Bound] deriving Show

gen_builtin_identifier :: Identifier -> Maybe String
gen_builtin_identifier (Identifier i) = fst <$> Map.lookup i builtins where
    builtins = builtin_subroutines <>  builtin_functions <> builtin_constants

typeof_builtin_identifier :: Identifier -> Maybe SoucType
typeof_builtin_identifier (Identifier i) = snd <$> Map.lookup i builtins where
    builtins = builtin_subroutines <> builtin_functions <> builtin_constants


gen_builtin_data :: String -> Maybe String
gen_builtin_data s = fst <$> Map.lookup s builtin_data

typeof_builtin_data :: String -> Maybe SoucType
typeof_builtin_data i = snd <$> Map.lookup i builtin_data

builtin_subroutines :: Mapping
builtin_subroutines = Map.fromList [
    ("puts", ("_souc_puts", (SoucRoutn (Just (SoucType "String")))))
    ,
    ("write", ("_souc_write", SoucRoutn (Just (SoucPair (SoucType "OutputStream") (SoucType "String")))))
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

builtin_data :: Mapping
builtin_data = Map.fromList [
    ("True", ("-1", SoucType "Bool"))
    ,
    ("False", ("0", SoucType "Bool"))
    ,
    ("None", ("_souc_none", SoucMaybe (SoucType "Integer")))
    ]
