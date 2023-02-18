module SurCC.Parser.TypeDefs (
    type_def,
) where

import Text.Parsec hiding (space, spaces, string, parse)
import Data.Function
import Data.Functor

import SurCC.Common (TypeDef(..), Term(..), Identifier(..), Bound(..))
import SurCC.Common.Parsing
import SurCC.Common.SoucTypes
import SurCC.Parser.Common (SurCParser)
import SurCC.Parser.Basics (identifier, indentation)

 -- fixme: others (empty, gadt, etc.)

type_def :: SurCParser TypeDef
type_def = do
    reserved "def" *> spaces
    unit_type <|> enum_type <|> struct_type


unit_type :: SurCParser TypeDef
unit_type = do
    name <- opener "unit"
    c <- identifier
    closer
    pure $ UnitType name c


enum_type :: SurCParser TypeDef
enum_type = do
    name <- opener "enum"
    constructors <- many1 (identifier <* spaces)
    closer
    pure $ EnumType name constructors


struct_type :: SurCParser TypeDef
struct_type = do
    name <- opener "struct"
    constructor <- identifier
    accessors <- many1 (accessor name)
    let constructor_t = (accessors <&> get_type) & foldr SoucFn name
            where
                get_type :: Bound -> SoucType
                get_type (Bound _ s) = s
    closer
    pure $ StructType name (Bound constructor constructor_t : accessors)

accessor :: SoucType -> SurCParser Bound
accessor t = do
    i <- char '(' *> optional spaces *> identifier
    sig <- type_signature <* optional spaces <* char ')'
    pure $ Bound i (SoucFn t sig)


opener :: String -> SurCParser SoucType
opener word =
    reserved word *> spaces *>
    type_name
    <* spaces <* char '=' <* spaces <* char '{' <* optional spaces

closer :: SurCParser ()
closer = optional spaces *> char '}' *> endline
