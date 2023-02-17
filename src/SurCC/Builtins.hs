module SurCC.Builtins (
    gen_builtin,
    typeof_builtin,
    builtin_identifiers,
    builtin_types,
) where

import Data.HashMap.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))

import SurCC.Common
import SurCC.Common.Hashable


type Mapping = Map.HashMap Identifier Builtin

data Builtin = Builtin {
    body :: Text,
    souc_type :: SoucType,
    cleanup :: Maybe Cleanup
}

newtype Cleanup = Cleanup Text


gen_builtin :: Identifier -> Maybe Text
gen_builtin i = body <$> Map.lookup i all_builtins where


typeof_builtin :: Identifier -> Maybe SoucType
typeof_builtin i = Map.lookup i builtin_identifiers where


all_builtins :: Mapping
all_builtins = builtin_subroutines <> builtin_functions <> builtin_constants


builtin_identifiers :: Map.HashMap Identifier SoucType
builtin_identifiers = Map.map souc_type all_builtins


builtin_types :: Map.HashMap SoucType Refutable
builtin_types = Map.fromList [
    (SoucBool, Refutable True),
    (SoucChar, Refutable True),
    (SoucInteger, Refutable True),
    (SoucString, Refutable True)
    ]


builtin_subroutines :: Mapping
builtin_subroutines = Map.fromList [
    ("puts",
        -- FIXME delete this eventually
        -- some type-checker tests will be complicated by removing this
        Builtin "_souc_puts"
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


builtin_functions :: Mapping
builtin_functions = Map.fromList [
    ("increment", Builtin "_souc_increment"
        (SoucFn SoucInteger SoucInteger)
        Nothing
    ),
    ("remainder", Builtin "_souc_remainder"
        (SoucFn (SoucPair SoucInteger SoucInteger) SoucInteger)
        Nothing
    ),
    ("str", Builtin "_souc_int_str"
        (SoucFn SoucInteger SoucString)
        Nothing
    )]


builtin_constants :: Mapping
builtin_constants = Map.fromList [
    ("pi", Builtin
        "(union _souc_obj) { ._souc_int = 3 }" -- biblical value
        SoucInteger
        Nothing
    ),
    ("true", Builtin
        "(union _souc_obj) { ._souc_bool = true }"
        SoucBool
        Nothing
    ),
    ("false", Builtin
        "(union _souc_obj) { ._souc_bool = false }"
        SoucBool
        Nothing
    ),
    ("none", Builtin
        "_souc_none"
        (SoucMaybe SoucInteger) -- FIXME make polymorphic
        Nothing
    ),
    ("ok", Builtin -- FIXME "some"?
        "_souc_ok"
        (SoucFn SoucInteger (SoucMaybe SoucInteger)) -- FIXME make polymorphic
        Nothing
    )]
