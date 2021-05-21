module CodeGen.Runtime.FuncDefs (funcdefs) where

import Data.List (intercalate)

funcdefs :: String
funcdefs = intercalate "\n" [
    tuple,
    suum,
    is_equal,
    is_lesser,
    increment
    ]

tuple = "union _souc_obj _souc_tuple(union _souc_obj x, union _souc_obj y) { struct _souc_pair * p = calloc(sizeof(*p), 1); p->first = x; p->second = y; return (union _souc_obj) { ._souc_pair = p }; }"
-- FIXME can this be done without calloc?

suum = "union _souc_obj _souc_sum(union _souc_obj x, union _souc_obj y) { return (union _souc_obj) { ._souc_int = x._souc_int + y._souc_int };}"

is_equal = "union _souc_obj _souc_is_equal(union _souc_obj x, union _souc_obj y) { union _souc_obj b; b._souc_bool = x._souc_int == y._souc_int; return b;}"

is_lesser = "union _souc_obj _souc_is_lesser(union _souc_obj x, union _souc_obj y) { union _souc_obj b; b._souc_bool = x._souc_int < y._souc_int; return b;}"

increment = "union _souc_obj _souc_increment(union _souc_obj n) { return (union _souc_obj) { ._souc_int = n._souc_int + 1 };}"
