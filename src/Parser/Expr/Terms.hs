-- term parsers
module Parser.Expr.Terms (
    parse_term_token
) where


import Data.Text qualified as Text
import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>))

import Common
import Common.Parsing

import Parser.Expr.Types
import Parser.Expr.RegardingSpaces

parse_term_token :: ShuntingYardParser TermToken
parse_term_token = parse_term_tok <|> parse_left_paren <|> parse_prefix_op

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
parse_num = LitInt <$> read <$> Parsec.many1 Parsec.digit

parse_var :: ShuntingYardParser Term
parse_var = do
    first <- Parsec.lower <|> Parsec.char '_'
    rest <- Parsec.many (Parsec.lower <|> Parsec.char '_' <|> Parsec.digit)
    pure $ Var $ Identifier $ Text.pack $ first:rest

parse_char :: ShuntingYardParser Term
parse_char = LitChar <$> ((Parsec.char '\'') *> Parsec.anyChar <* (Parsec.char '\''))

parse_string :: ShuntingYardParser Term
parse_string = LitString . Text.pack <$> ((Parsec.char '\"') *> Parsec.many (Parsec.noneOf "\"") <* (Parsec.char '\"'))

parse_constructor :: ShuntingYardParser Term
parse_constructor = Constructor <$> Constant <$> upper_name

parse_left_paren :: ShuntingYardParser TermToken
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> pure ()))
    ignore_spaces *> Parsec.char '(' *> pure LParen
