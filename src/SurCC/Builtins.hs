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


type Mapping a = Map.HashMap a Builtin

data Builtin = Builtin {
    body :: Text,
    souc_type :: SoucType,
    cleanup :: Maybe Cleanup
}

newtype Cleanup = Cleanup Text

newtype BuiltinsCtx = Builtins [Bound]

instance TextShow BuiltinsCtx where
    showb list = "Builtins" <> showb list

typeof_builtin :: Either Identifier Constant -> Maybe SoucType
typeof_builtin = \case
    Left i -> typeof_builtin_identifier i
    Right c -> typeof_builtin_data c

gen_builtin_identifier :: Identifier -> Maybe Text
gen_builtin_identifier i = body <$> Map.lookup i builtins where
    builtins :: Mapping Identifier
    builtins = builtin_subroutines <>  builtin_functions <> builtin_constants

typeof_builtin_identifier :: Identifier -> Maybe SoucType
typeof_builtin_identifier i = souc_type <$> Map.lookup i builtins where
    builtins :: Mapping Identifier
    builtins = builtin_subroutines <> builtin_functions <> builtin_constants


gen_builtin_data :: Constant -> Maybe Text
gen_builtin_data c = body <$> Map.lookup c builtin_data

typeof_builtin_data :: Constant -> Maybe SoucType
typeof_builtin_data c = souc_type <$> Map.lookup c builtin_data

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
    )]

builtin_data :: Mapping Constant
builtin_data = Map.fromList [
    ("True", Builtin
        "(union _souc_obj) { ._souc_bool = true }"
        (SoucType "Bool" (SoucKind 0))
        Nothing
    ),
    ("False", Builtin
        "(union _souc_obj) { ._souc_bool = false }"
        (SoucType "Bool" (SoucKind 0))
        Nothing
    ),
    ("None", Builtin
        "_souc_none"
        (SoucMaybe SoucInteger)
        Nothing
    ),
    ("OK", Builtin
        "_souc_ok"
        (SoucFn SoucInteger (SoucMaybe SoucInteger))
        Nothing
    )]
