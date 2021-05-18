module CodeGen.Runtime.TypeDefs (typedefs) where

import Data.List (intercalate)

typedefs :: String
typedefs = intercalate "\n" [
    souc_obj,
    souc_maybe
    ]

souc_obj = "union _souc_obj { int _souc_int ; char _souc_char ; char * _souc_str; };"

souc_maybe = "struct _souc_maybe { int has_val; union _souc_obj val; };"
