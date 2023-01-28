-- simple spacing-related parsers

module SurCC.Parser.Expr.RegardingSpaces (
    get_tightness,
    set_spacing_tight,
    respect_spaces,
    ignore_spaces,
    silent_space,
    no_spaces,
    if_loosely_spaced,
    if_tightly_spaced
) where

import Control.Monad (when)
import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>))

import SurCC.Parser.Expr.Types

get_tightness :: ShuntingYardParser Tightness
get_tightness = do
    (_, _, tightness, _) <- Parsec.getState
    pure tightness


set_spacing_tight :: Bool -> ShuntingYardParser ()
set_spacing_tight b = Parsec.modifyState (\(s1,s2,_,s3) -> (s1, s2, Tight b,s3))

respect_spaces :: ShuntingYardParser ()
respect_spaces = Parsec.skipMany1 silent_space

ignore_spaces :: ShuntingYardParser ()
ignore_spaces = Parsec.skipMany silent_space

silent_space :: ShuntingYardParser Char
silent_space = Parsec.char ' ' <?> ""

no_spaces :: String -> ShuntingYardParser ()
no_spaces failmsg = Parsec.try ((Parsec.try silent_space *> Parsec.unexpected failmsg) <|> pure ())

if_loosely_spaced :: ShuntingYardParser () -> ShuntingYardParser ()
if_loosely_spaced action = do
    Tight spaced <- get_tightness
    when (not spaced) action

if_tightly_spaced :: ShuntingYardParser () -> ShuntingYardParser ()
if_tightly_spaced action = do
    Tight spaced <- get_tightness
    when spaced action
