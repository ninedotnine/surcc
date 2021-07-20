module CodeGen.Runtime.SubrDefs (subrdefs) where

import Data.Text (Text)
import qualified Data.Text as Text

subrdefs :: Text
subrdefs = Text.intercalate "\n" [write, abort, "// end of subrdefs\n"]

write :: Text
write = "void _souc_write(union _souc_obj pair) { puts(pair._souc_pair->second._souc_str); } "

abort :: Text
abort = "void _souc_abort(void) { abort(); } "

puts :: Text
puts = "void _souc_puts(union _souc_obj val) { puts(val._souc_str); } "
