module SurCC.CodeGen.Runtime (runtime) where

import SurCC.CodeGen.Runtime.TypeDefs (typedefs)
import SurCC.CodeGen.Runtime.FuncDefs (funcdefs)
import SurCC.CodeGen.Runtime.SubrDefs (subrdefs)
import SurCC.CodeGen.Runtime.DataDefs (datadefs)

import Data.Text (Text)
import Data.Text qualified as Text

runtime :: Text
runtime = Text.unlines [includes, typedefs, funcdefs, subrdefs, datadefs]

includes :: Text
includes = "#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>\n#include <string.h>\n"
