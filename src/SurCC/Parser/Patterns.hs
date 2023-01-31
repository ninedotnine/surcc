module SurCC.Parser.Patterns (
    parse_pattern,
) where

import Data.Functor
import Data.Text
import Text.Parsec

import SurCC.Common
import SurCC.Common.Parsing (literal)

import SurCC.Parser.Basics (identifier)
import SurCC.Parser.Expr.StackManipulations
import SurCC.Parser.Expr.RegardingSpaces
import SurCC.Parser.Expr.Terms
import SurCC.Parser.Expr.Opers

parse_pattern :: Parsec Text s Pattern
parse_pattern = parse_pat_lit <|> parse_pat_var
    where
        parse_pat_lit = literal <&> PatLit
        parse_pat_var = identifier <&> PatBinding
