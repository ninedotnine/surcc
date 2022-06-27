module SurCC.CodeGen.Runtime.SubrDefs (subrdefs) where

import Data.Text (Text)
import Data.Text qualified as Text

subrdefs :: Text
subrdefs = Text.intercalate "\n" [write, abort, "// end of subrdefs\n"]

write :: Text
write = "void _souc_write(union _souc_obj pair) { puts(pair._souc_pair->second._souc_str); } "

abort :: Text
abort = "void _souc_abort(void) { abort(); } "
