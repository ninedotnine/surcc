module SurCC.Parser.TypeDefs (
    type_def,
) where

import Text.Parsec hiding (space, spaces, string, parse)

import SurCC.Common (TypeDef(..), Term(..), Identifier(..))
import SurCC.Common.Parsing
import SurCC.Parser.Common (SurCParser)
import SurCC.Parser.Basics (identifier, indentation)

 -- fixme: others (empty, struct, gadt, etc.)

type_def :: SurCParser TypeDef
type_def = do
    reserved "def" *> spaces
    unit_type <|> enum_type

unit_type :: SurCParser TypeDef
unit_type = do
    reserved "unit" *> spaces
    name <- type_name
    spaces *> char '=' *> spaces *> char '{' *> optional spaces
    c <- identifier
    optional spaces *> char '}' *> endline
    pure $ UnitType name c

enum_type :: SurCParser TypeDef
enum_type = do
    reserved "enum" *> spaces
    name <- type_name
    spaces *> char '=' *> spaces *> char '{' *> optional spaces
    constructors <- many1 (identifier <* spaces)
    optional spaces *> char '}' *> endline
    pure $ EnumType name constructors
