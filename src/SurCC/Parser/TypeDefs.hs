module SurCC.Parser.TypeDefs (
    type_def,
) where

import Text.Parsec hiding (space, spaces, string, parse)

import SurCC.Common (TypeDef(..), Term(..), Constant(..))
import SurCC.Common.Parsing
import SurCC.Parser.Common (SurCParser)

 -- fixme: others (empty, struct, gadt, etc.)

type_def :: SurCParser TypeDef
type_def = do
    try (reserved "def") *> spaces
    unit_type <|> enum_type

unit_type :: SurCParser TypeDef
unit_type = do
    try (reserved "unit") *> spaces
    name <- type_name
    spaces *> char '=' *> spaces *> char '{' *> optional spaces
    c <- constructor
    optional spaces *> char '}' *> endline
    pure $ UnitType name c

enum_type :: SurCParser TypeDef
enum_type = do
    try (reserved "enum") *> spaces
    name <- type_name
    spaces *> char '=' *> spaces *> char '{' *> optional spaces
    constructors <- many1 (constructor <* spaces)
    optional spaces *> char '}' *> endline
    pure $ EnumType name constructors

constructor :: SurCParser Term
constructor = Constructor <$> Constant <$> upper_name
