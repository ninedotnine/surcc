module Parser.Common (
    ParserState,
    SouCParser
--     Bindings,
--     Indentation
) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Text (Text)
import Data.Map.Strict qualified as Map (Map, empty)
import Text.Parsec (Parsec)

import Common

-- FIXME: should this be a list of maps (for levels of scope)?
type ParserState = (Indentation, NonEmpty Bindings)

type SouCParser a = Parsec Text ParserState a

type Indentation = Int -- for now, indentation must be exactly one tab

type Bindings = Map.Map Identifier Mutability
