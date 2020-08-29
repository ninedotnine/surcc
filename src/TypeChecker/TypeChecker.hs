module TypeChecker.TypeChecker (
    type_check,
    ) where

import Common

import Debug.Trace

data Bound = Bound Identifier TypeName
    deriving Show

data Context = Global [Bound]
             | Scoped [Bound] Context
    deriving Show

-- FIXME this should fail sometimes lol
type_check :: Program -> Either TypeError CheckedProgram
type_check (Program name imports defns) = do
    traceM $ "U!!" ++ show (get_top_level_identifiers (Program name imports defns))
    Right $ CheckedProgram name imports defns

get_top_level_identifiers :: Program -> Context
get_top_level_identifiers (Program _ imports defns) = Global $ get_imports imports



get_imports :: Imports -> [Bound]
get_imports imports = map make_import_bound (map from_import imports)
    where
        from_import :: Import -> String
        from_import (Import s) = s
        make_import_bound s = Bound (Identifier s) (TypeName "Module")
