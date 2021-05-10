module CodeGen.Runtime (runtime) where

import CodeGen.Runtime.TypeDefs (typedefs)
import CodeGen.Runtime.FuncDefs (funcdefs)
import CodeGen.Runtime.SubrDefs (subrdefs)

import Data.List (intercalate)

runtime :: String
runtime = intercalate "\n" [includes, typedefs, funcdefs, subrdefs]

includes :: String
includes = "#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>\n"
