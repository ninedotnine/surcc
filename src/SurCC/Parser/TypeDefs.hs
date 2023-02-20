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
    unit_type <|> enum_type <|> isomorphism_type <|> struct_type


unit_type :: SurCParser TypeDef
unit_type = do
    name <- opener "unit" souc_type_simple
    c <- identifier
    closer
    pure $ UnitType name c


enum_type :: SurCParser TypeDef
enum_type = do
    name <- opener "enum" souc_type_simple
    constructors <- many1 (identifier <* spaces)
    closer
    pure $ EnumType name constructors


isomorphism_type :: SurCParser TypeDef
isomorphism_type = do
    name <- opener "isomorphism" souc_type_parameterized
    constructor <- identifier
    (accessr_id,accessr_return_t) <- accessor
    let
        constructor_t = SoucFn accessr_return_t name
        accessr = if accessr_id == "_"
            then Nothing
            else Just $ Bound accessr_id (SoucFn name accessr_return_t)
    closer
    pure $ IsomorphismType name constructor constructor_t accessr


struct_type :: SurCParser TypeDef
struct_type = do
    name <- opener "struct" souc_type_parameterized
    constructor <- identifier
    accessors <- many1 accessor
    let
        constructor_t = accessors <&> snd & foldr SoucFn name
        accessor_bounds = accessors
                          & filter (\(i,_) ->  i /= "_")
                          <&> (\(i,t) -> Bound i (SoucFn name t))

    closer
    pure $ StructType name (Bound constructor constructor_t : accessor_bounds)


accessor :: SurCParser (Identifier,SoucType)
accessor = do
    i <- char '(' *> optional spaces *> identifier
    sig <- type_signature <* optional spaces <* char ')'
    pure (i,sig)


opener :: String -> SurCParser SoucType -> SurCParser SoucType
opener word name_parser =
    reserved word *> spaces *>
    name_parser
    <* spaces <* char '=' <* spaces <* char '{' <* optional spaces

closer :: SurCParser ()
closer = optional spaces *> char '}' *> endline
