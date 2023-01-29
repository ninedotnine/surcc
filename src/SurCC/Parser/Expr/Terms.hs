-- term parsers
module SurCC.Parser.Expr.Terms (
    parse_term_token,
    parse_var
) where


import Data.Text qualified as Text
import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>))

import SurCC.Common
import SurCC.Common.Parsing

import SurCC.Parser.Basics (identifier)
import SurCC.Parser.Expr.Types
import SurCC.Parser.Expr.RegardingSpaces

parse_term_token :: ShuntingYardParser TermToken
parse_term_token = parse_term_tok <|> parse_left_paren <|> parse_prefix_op


-- FIXME : delete these and use the ones from Common/Parsing.hs ?
parse_term_tok :: ShuntingYardParser TermToken
parse_term_tok = TermTok <$> (
        parse_num
    <|> parse_char
    <|> parse_string
    <|> parse_constructor
    <|> parse_var)

parse_prefix_op :: ShuntingYardParser TermToken
parse_prefix_op = do
    oper <- parse_oper_symbol
    spacing <- Parsec.optionMaybe respect_spaces
    case spacing of
        Nothing -> pure (TightPreOp oper)
        Just () -> pure (SpacedPreOp oper)
    where parse_oper_symbol = (
            Parsec.char '!' *> pure Deref   <|>
            Parsec.char '@' *> pure GetAddr <|>
            Parsec.char '~' *> pure Negate  <|>
            Parsec.char '$' *> pure ToString
            ) <?> "prefix operator"

parse_num :: ShuntingYardParser Term
parse_num = Lit <$> LitInt <$> read <$> Parsec.many1 Parsec.digit

parse_var :: ShuntingYardParser Term
parse_var = do
    i <- identifier
    pure $ Var i

parse_char :: ShuntingYardParser Term
parse_char = Lit <$> LitChar <$> ((Parsec.char '\'') *> Parsec.anyChar <* (Parsec.char '\''))

parse_string :: ShuntingYardParser Term
parse_string = Lit <$> LitString . Text.pack <$> ((Parsec.char '\"') *> Parsec.many (Parsec.noneOf "\"") <* (Parsec.char '\"'))

parse_constructor :: ShuntingYardParser Term
parse_constructor = Constructor <$> Constant <$> upper_name

parse_left_paren :: ShuntingYardParser TermToken
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> pure ()))
    ignore_spaces *> Parsec.char '(' *> pure LParen
