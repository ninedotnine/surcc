module TypeChecker.TypeChecker (
    type_check,
    ) where

import Common

-- FIXME this should fail sometimes lol
type_check :: Program -> Either TypeError CheckedProgram
type_check (Program name imports defns) = Right $ CheckedProgram name imports defns
