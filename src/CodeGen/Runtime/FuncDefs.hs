module CodeGen.Runtime.FuncDefs (funcdefs) where

import Data.List (intercalate)

funcdefs :: String
funcdefs = intercalate "\n" [
    tuple,
    increment
    ]

tuple = " struct _souc_obj _souc_tuple(int unused, char * msg) { return (struct _souc_obj) { .type = _souc_str , .val._souc_str = msg }; }"

increment = " int _souc_increment(int n) { return n+1;} "
