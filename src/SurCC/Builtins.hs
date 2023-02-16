module SurCC.Builtins (
    gen_builtin,
    typeof_builtin,
) where

import Data.HashMap.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))

import SurCC.Common
import SurCC.Common.Hashable


type Mapping a = Map.HashMap a Builtin

data Builtin = Builtin {
    body :: Text,
    souc_type :: SoucType,
    cleanup :: Maybe Cleanup
}

newtype Cleanup = Cleanup Text


gen_builtin :: Identifier -> Maybe Text
gen_builtin i = body <$> Map.lookup i all_builtins where


typeof_builtin :: Identifier -> Maybe SoucType
typeof_builtin i = souc_type <$> Map.lookup i all_builtins where


all_builtins :: Mapping Identifier
all_builtins = builtin_subroutines <> builtin_functions <> builtin_constants


builtin_subroutines :: Mapping Identifier
builtin_subroutines = Map.fromList [
    ("puts", Builtin "_souc_puts"
        (SoucRoutn SoucString)
        Nothing
    ),
    ("write", Builtin "_souc_write"
        (SoucRoutn
            (SoucPair (SoucType "OutputStream" (SoucKind 0)) SoucString))
        Nothing
    ),
    ("abort", Builtin "abort"
        SoucIO
        Nothing
    )]


builtin_functions :: Mapping Identifier
builtin_functions = Map.fromList [
    ("increment", Builtin "_souc_increment"
        (SoucFn SoucInteger SoucInteger)
        Nothing
    ),
    ("str", Builtin "_souc_int_str"
        (SoucFn SoucInteger SoucString)
        Nothing
    )]


builtin_constants :: Mapping Identifier
builtin_constants = Map.fromList [
    ("pi", Builtin
        "(union _souc_obj) { ._souc_int = 3 }" -- biblical value
        SoucInteger
        Nothing
    ),
    ("i42", Builtin
        "_souc_42"
        SoucInteger
        Nothing
    ),
    ("ok43", Builtin
        "_souc_42"
        SoucInteger
        Nothing
    ),
    ("true", Builtin
        "(union _souc_obj) { ._souc_bool = true }"
        (SoucType "Bool" (SoucKind 0))
        Nothing
    ),
    ("false", Builtin
        "(union _souc_obj) { ._souc_bool = false }"
        (SoucType "Bool" (SoucKind 0))
        Nothing
    ),
    ("none", Builtin
        "_souc_none"
        (SoucMaybe SoucInteger)
        Nothing
    ),
    ("ok", Builtin
        "_souc_ok"
        (SoucFn SoucInteger (SoucMaybe SoucInteger))
        Nothing
    )]
