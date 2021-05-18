module CodeGen.Builtins (
    gen_builtin_identifier,
    gen_builtin_data,
    typeof_builtin_identifier,
    BuiltinsCtx(..),
) where

import Common
import Parser.ExprParser

import qualified Data.HashMap.Strict as Map

-- type Mapping = Map.HashMap Identifier (String, SoucType)
type Mapping = Map.HashMap String (String, SoucType)

data BuiltinFunction = BuiltinFunction Identifier String SoucType

data BuiltinSubroutine = BuiltinSubroutine Identifier String SoucType

data BuiltinConstant = BuiltinConstant Identifier String SoucType

data BuiltinData = BuiltinData Identifier String SoucType


gen_builtin_identifier :: Identifier -> Maybe String
gen_builtin_identifier (Identifier name) = case Map.lookup name builtins of
    Nothing -> Just ("_souc_" <> name)
    just_something -> fst <$> just_something
    where
        builtins = builtin_subroutines <>  builtin_functions <> builtin_constants

gen_builtin_constant :: Identifier -> Maybe String
gen_builtin_constant (Identifier i) = fst <$> Map.lookup i builtin_constants

gen_builtin_data :: String -> Maybe String
gen_builtin_data s = fst <$> Map.lookup s builtin_data


typeof_builtin_constant :: String -> Maybe SoucType
typeof_builtin_constant i = snd <$> Map.lookup i builtin_data

typeof_builtin_identifier :: Identifier -> Maybe SoucType
-- typeof_builtin_identifier i = snd <$> Map.lookup i builtin_constants
typeof_builtin_identifier (Identifier i) = snd <$> Map.lookup i builtins where
    builtins = builtin_subroutines <> builtin_functions <> builtin_constants

typeof_builtin_subroutine :: Identifier -> Maybe SoucType
typeof_builtin_subroutine (Identifier i) = snd <$> Map.lookup i builtin_subroutines

typeof_builtin_function :: Identifier -> Maybe SoucType
typeof_builtin_function (Identifier i) = snd <$> Map.lookup i builtin_functions


newtype BuiltinsCtx = Builtins [Bound] deriving Show

builtins_ctx :: BuiltinsCtx
builtins_ctx = Builtins [
    Bound "puts" (SoucRoutn (Just (SoucType "String"))),
    Bound "abort" (SoucRoutn Nothing),
    Bound "write" (SoucRoutn (Just (SoucPair (SoucType "OutputStream") (SoucType "String"))))
    ]
-- builtins_ctx = b
-- builtins_ctx = undefined



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

-- builtin_constants :: Mapping
builtin_constants :: Map.HashMap String (String , SoucType)
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
