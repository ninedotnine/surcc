module CodeGen.Runtime.FuncDefs (funcdefs) where

import Data.List (intercalate)

funcdefs :: String
funcdefs = intercalate "\n" [
    tuple,
    increment
    ]

tuple = "union _souc_obj _souc_tuple(union _souc_obj x, union _souc_obj y) { struct _souc_pair * p = calloc(sizeof(*p), 1); p->first = x; p->second = y; return (union _souc_obj) { ._souc_pair = p }; }"

increment = " int _souc_increment(int n) { return n+1;} "
