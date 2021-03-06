module SurCC.Builtins (
    gen_builtin_identifier,
    gen_builtin_data,
    typeof_builtin,
    typeof_builtin_identifier,
    typeof_builtin_data,
) where

import Data.HashMap.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))

import SurCC.Common


type Mapping a = Map.HashMap a (Text, SoucType)

newtype BuiltinsCtx = Builtins [Bound]

instance TextShow BuiltinsCtx where
    showb list = "Builtins" <> showb list

typeof_builtin :: Either Identifier Constant -> Maybe SoucType
typeof_builtin = \case
    Left i -> typeof_builtin_identifier i
    Right c -> typeof_builtin_data c

gen_builtin_identifier :: Identifier -> Maybe Text
gen_builtin_identifier i = fst <$> Map.lookup i builtins where
    builtins = builtin_subroutines <>  builtin_functions <> builtin_constants

typeof_builtin_identifier :: Identifier -> Maybe SoucType
typeof_builtin_identifier i = snd <$> Map.lookup i builtins where
    builtins = builtin_subroutines <> builtin_functions <> builtin_constants


gen_builtin_data :: Constant -> Maybe Text
gen_builtin_data c = fst <$> Map.lookup c builtin_data

typeof_builtin_data :: Constant -> Maybe SoucType
typeof_builtin_data c = snd <$> Map.lookup c builtin_data

builtin_subroutines :: Mapping Identifier
builtin_subroutines = Map.fromList [
    ("puts", ("_souc_puts", (SoucRoutn SoucString)))
    ,
    ("write", ("_souc_write", SoucRoutn (SoucPair (SoucType "OutputStream" (SoucKind 0)) SoucString)))
    ,
    ("abort", ("abort", SoucIO))
    ]

builtin_functions :: Mapping Identifier
builtin_functions = Map.fromList [
    ("increment", ("_souc_increment", SoucFn SoucInteger SoucInteger))
    ,
    ("str", ("_souc_int_str", SoucFn SoucInteger SoucString))
    ]

builtin_constants :: Mapping Identifier
builtin_constants = Map.fromList [
    ("pi", ("(union _souc_obj) { ._souc_int = 3 }", SoucInteger)) -- biblical value
    ,
    ("i42", ("_souc_42", SoucInteger))
    ,
    ("ok43", ("_souc_42", SoucInteger))
    ]

builtin_data :: Mapping Constant
builtin_data = Map.fromList [
    ("True", ("(union _souc_obj) { ._souc_bool = true }", SoucType "Bool" (SoucKind 0)))
    ,
    ("False", ("(union _souc_obj) { ._souc_bool = false }", SoucType "Bool" (SoucKind 0)))
    ,
    ("None", ("_souc_none", SoucMaybe SoucInteger))
    ,
    ("OK", ("_souc_ok", SoucFn SoucInteger (SoucMaybe SoucInteger)))
    ]
