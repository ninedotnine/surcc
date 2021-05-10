module CodeGen.Runtime.FuncDefs (funcdefs) where

funcdefs :: String
funcdefs = concat [tuple]

tuple = " struct _souc_obj _souc_tuple(int unused, char * msg) { return (struct _souc_obj) { .type = _souc_str , .val._souc_str = msg }; }"
