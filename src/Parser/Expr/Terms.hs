-- term parsers
module Expr.Terms (
    parse_term_token
) where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), (<?>))

import Expr.ExprTypes
import Expr.RegardingSpaces

parse_term_token :: ShuntingYardParser TermToken
parse_term_token = parse_term_tok <|> parse_left_paren <|> parse_prefix_op

parse_term_tok :: ShuntingYardParser TermToken
parse_term_tok = TermTok <$> (parse_num <|> parse_char <|> parse_string <|> parse_var)


parse_prefix_op :: ShuntingYardParser TermToken
parse_prefix_op = do
    oper <- parse_oper_symbol
    spacing <- Parsec.optionMaybe respect_spaces
    case spacing of
        Nothing -> return (TightPreOp oper)
        Just () -> return (SpacedPreOp oper)
    where parse_oper_symbol = (
            Parsec.char '!' *> return Deref   <|>
            Parsec.char '@' *> return GetAddr <|>
            Parsec.char '~' *> return Negate  <|>
            Parsec.char '$' *> return ToString
            ) <?> "prefix operator"

parse_num :: ShuntingYardParser Term
parse_num = Lit <$> read <$> Parsec.many1 Parsec.digit

parse_var :: ShuntingYardParser Term
parse_var = do
    first <- Parsec.lower <|> Parsec.char '_'
    rest <- Parsec.many (Parsec.lower <|> Parsec.char '_' <|> Parsec.digit)
    return $ Var (first:rest)

parse_char :: ShuntingYardParser Term
parse_char = CharLit <$> ((Parsec.char '\'') *> Parsec.anyChar <* (Parsec.char '\''))

parse_string :: ShuntingYardParser Term
parse_string = StringLit <$> ((Parsec.char '\"') *> Parsec.many (Parsec.noneOf "\"") <* (Parsec.char '\"'))

parse_left_paren :: ShuntingYardParser TermToken
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> return ()))
    ignore_spaces *> Parsec.char '(' *> return LParen


