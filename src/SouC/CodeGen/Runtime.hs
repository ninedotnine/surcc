module SouC.CodeGen.Runtime (runtime) where

import SouC.CodeGen.Runtime.TypeDefs (typedefs)
import SouC.CodeGen.Runtime.FuncDefs (funcdefs)
import SouC.CodeGen.Runtime.SubrDefs (subrdefs)
import SouC.CodeGen.Runtime.DataDefs (datadefs)

import Data.Text (Text)
import Data.Text qualified as Text

runtime :: Text
runtime = Text.unlines [includes, typedefs, funcdefs, subrdefs, datadefs]

includes :: Text
includes = "#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>\n"
