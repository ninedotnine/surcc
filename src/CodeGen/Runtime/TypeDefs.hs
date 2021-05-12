module CodeGen.Runtime.TypeDefs (typedefs) where

import Data.List (intercalate)

typedefs :: String
typedefs = intercalate "\n" [
    souc_type_tag,
    souc_val,
    souc_obj,
    souc_maybe,
    souc_none
    ]

souc_type_tag = "enum _souc_type_tag { _souc_int , _souc_char , _souc_str };"

souc_val = "union _souc_val { int _souc_int ; char _souc_char ; char * _souc_str; };"

souc_obj = "struct _souc_obj { enum _souc_type_tag type ; union _souc_val val; };"

souc_maybe = "struct _souc_maybe { int has_val; struct _souc_obj val; };"

souc_none = "struct _souc_maybe _souc_none = { 0 };"
