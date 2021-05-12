module CodeGen.Runtime.SubrDefs (subrdefs) where

import Data.List (intercalate)

subrdefs :: String
subrdefs = intercalate "\n" [write, abort]

write :: String
write = "void _souc_write(struct _souc_obj obj) { puts(obj.val._souc_str); } "

abort :: String
abort = "void _souc_abort(void) { abort(); } "

puts :: String
puts = "void _souc_puts(struct _souc_obj obj) { puts(obj.val._souc_str); } "
