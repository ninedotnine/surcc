module Parser.TypeDefs (
    type_def,
) where

import Text.Parsec hiding (space, spaces, string, parse)

import Common (TypeDef(..))
import Common.Parsing
import Parser.Common (SouCParser)

 -- fixme: others (empty, enum, struct, gadt, etc.)

type_def :: SouCParser TypeDef
type_def = do
    reserved "def" *> spaces
    unit_type

unit_type :: SouCParser TypeDef
unit_type = do
    reserved "unit" *> spaces
    name <- type_name
    spaces *> char '=' *> spaces *> char '{' *> optional spaces
    constructor <- constructor_name
    optional spaces *> char '}' *> endline
    pure $ UnitType name constructor

