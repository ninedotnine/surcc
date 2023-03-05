module SurCC.Parser.TypeDefs (
    type_def,
) where

import Text.Parsec hiding (space, spaces, string, parse, newline)
import Control.Applicative (liftA2)
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
    c <- in_braces identifier
    pure $ UnitType name c


enum_type :: SurCParser TypeDef
enum_type = do
    name <- opener "enum" souc_type_simple
    constructors <- in_braces $ multiple identifier
    pure $ EnumType name constructors


isomorphism_type :: SurCParser TypeDef
isomorphism_type = do
    (name,args) <- opener "isomorphism" souc_type_parameterized
    (constructor,accessr_id,accessr_return_t) <- in_braces $ do
        i <- identifier
        (aid,art) <- accessor
        pure (i,aid,art)
    let
        constructor_t = SoucFn accessr_return_t (SoucTypeCon name args)
        accessr = if accessr_id == "_"
            then Nothing
            else Just $ Bound accessr_id (
                            SoucFn (SoucTypeCon name args) accessr_return_t)
    pure $ IsomorphismType name constructor constructor_t accessr


struct_type :: SurCParser TypeDef
struct_type = do
    (name,args) <- opener "struct" souc_type_parameterized
    (constructor,accessors) <- in_braces $
        liftA2 (,) identifier (many1 accessor)
    let
        constructor_t = accessors <&> snd & foldr SoucFn (SoucTypeCon name args)
        accessor_bounds = accessors
                          & filter (\(i,_) ->  i /= "_")
                          <&> (\(i,t) -> Bound i (
                                            SoucFn (SoucTypeCon name args) t))
    pure $ StructType name (Bound constructor constructor_t : accessor_bounds)


accessor :: SurCParser (Identifier,SoucType)
accessor = do
    i <- char '(' *> optional enclosed_spaces *> identifier
    sig <- type_signature <* optional enclosed_spaces <* char ')'
    pure (i,sig)


opener :: String -> SurCParser a -> SurCParser a
opener word name_parser =
    reserved word *> spaces *> name_parser <* spaces <* char '='


multiple :: SurCParser a -> SurCParser [a]
multiple p = p `sepBy1` try (enclosed_spaces *> lookAhead identifier_char)


in_braces :: SurCParser a -> SurCParser a
in_braces p = do
    void $ spaces *> char '{' *> optional enclosed_spaces
    x <- p
    void $ ignore_spaces *> optional newline *> char '}'
    pure x
