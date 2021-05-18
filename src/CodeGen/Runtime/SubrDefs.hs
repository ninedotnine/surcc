module CodeGen.Runtime.SubrDefs (subrdefs) where

import Data.List (intercalate)

subrdefs :: String
subrdefs = intercalate "\n" [write, abort]

write :: String
write = "void _souc_write(union _souc_obj val) { puts(val._souc_str); } "

abort :: String
abort = "void _souc_abort(void) { abort(); } "

puts :: String
puts = "void _souc_puts(union _souc_obj val) { puts(val._souc_str); } "
