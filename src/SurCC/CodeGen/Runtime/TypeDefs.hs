module SurCC.CodeGen.Runtime.TypeDefs (typedefs) where

import Data.Text (Text)
import Data.Text qualified as Text

typedefs :: Text
typedefs = Text.intercalate "\n" [
    souc_obj,
    souc_maybe,
    souc_pair,
    "// end of typedefs\n"
    ]


-- all these things should probably have `const`s

souc_obj = "union _souc_obj { int _souc_int; char _souc_char; bool _souc_bool; char * _souc_str; struct _souc_pair * _souc_pair; };"

souc_maybe = "struct _souc_maybe { int has_val; union _souc_obj val; };"

souc_pair = "struct _souc_pair { union _souc_obj first; union _souc_obj second; };"
