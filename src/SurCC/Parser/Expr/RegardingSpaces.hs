-- simple spacing-related parsers

module SurCC.Parser.Expr.RegardingSpaces (
    get_tightness,
    set_spacing_tight,
    no_spaces,
    if_loosely_spaced,
    if_tightly_spaced
) where

import Control.Monad (when)
import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>))

import SurCC.Parser.Expr.Types
import SurCC.Common.Parsing (space)

get_tightness :: ShuntingYardParser Tightness
get_tightness = do
    (_, _, tightness) <- Parsec.getState
    pure tightness


set_spacing_tight :: Bool -> ShuntingYardParser ()
set_spacing_tight b = Parsec.modifyState (\(s1,s2,_) -> (s1, s2, Tight b))

no_spaces :: String -> ShuntingYardParser ()
no_spaces failmsg = Parsec.try ((Parsec.try space *> Parsec.unexpected failmsg) <|> pure ())

if_loosely_spaced :: ShuntingYardParser () -> ShuntingYardParser ()
if_loosely_spaced action = do
    Tight spaced <- get_tightness
    when (not spaced) action

if_tightly_spaced :: ShuntingYardParser () -> ShuntingYardParser ()
if_tightly_spaced action = do
    Tight spaced <- get_tightness
    when spaced action
