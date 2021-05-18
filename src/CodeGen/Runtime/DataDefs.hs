module CodeGen.Runtime.DataDefs (datadefs) where

import Data.List (intercalate)

datadefs :: String
datadefs = intercalate "\n" [
    souc_none,
    souc_ok,
    souc_42,
    souc_ok43
    ]

souc_none = "struct _souc_maybe _souc_none = { 0 };"

souc_ok = "struct _souc_maybe _souc_ok(union _souc_obj v) { return (struct _souc_maybe) { .has_val = 1 , .val = v }; }"

souc_42 = "union _souc_obj _souc_42 = { ._souc_int = 42 };"

souc_ok43 = "struct _souc_maybe _souc_43 = { .has_val = 1, .val._souc_int = 43 };"
