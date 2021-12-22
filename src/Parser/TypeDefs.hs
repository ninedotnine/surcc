module Parser.TypeDefs (
    type_def,
) where

import Text.Parsec hiding (space, spaces, string, parse)

import Common (TypeDef(..), Term(..), Constant(..))
import Common.Parsing
import Parser.Common (SouCParser)

 -- fixme: others (empty, struct, gadt, etc.)

type_def :: SouCParser TypeDef
type_def = do
    reserved "def" *> spaces
    unit_type <|> enum_type

unit_type :: SouCParser TypeDef
unit_type = do
    reserved "unit" *> spaces
    name <- type_name
    spaces *> char '=' *> spaces *> char '{' *> optional spaces
    c <- constructor
    optional spaces *> char '}' *> endline
    pure $ UnitType name c

enum_type :: SouCParser TypeDef
enum_type = do
    reserved "enum" *> spaces
    name <- type_name
    spaces *> char '=' *> spaces *> char '{' *> optional spaces
    constructors <- many1 (constructor <* spaces)
    optional spaces *> char '}' *> endline
    pure $ EnumType name constructors

constructor :: SouCParser Term
constructor = Constructor <$> Constant <$> upper_name
