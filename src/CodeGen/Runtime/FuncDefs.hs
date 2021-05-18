module CodeGen.Runtime.FuncDefs (funcdefs) where

import Data.List (intercalate)

funcdefs :: String
funcdefs = intercalate "\n" [
    tuple,
    increment
    ]

tuple = "union _souc_obj _souc_tuple(union _souc_obj unused, char * msg) { return (union _souc_obj) { ._souc_str = msg }; }"

increment = " int _souc_increment(int n) { return n+1;} "
