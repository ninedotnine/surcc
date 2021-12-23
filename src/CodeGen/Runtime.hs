module CodeGen.Runtime (runtime) where

import CodeGen.Runtime.TypeDefs (typedefs)
import CodeGen.Runtime.FuncDefs (funcdefs)
import CodeGen.Runtime.SubrDefs (subrdefs)
import CodeGen.Runtime.DataDefs (datadefs)

import Data.Text (Text)
import Data.Text qualified as Text

runtime :: Text
runtime = Text.unlines [includes, typedefs, funcdefs, subrdefs, datadefs]

includes :: Text
includes = "#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>\n"
