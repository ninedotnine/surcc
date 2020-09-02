-- term parsers
module Parser.Expr.Terms (
    parse_term_token
) where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), (<?>))
import Common

import Parser.Expr.ExprTypes
import Parser.Expr.RegardingSpaces

parse_term_token :: ShuntingYardParser TermToken
parse_term_token = parse_term_tok <|> parse_left_paren <|> parse_prefix_op

parse_term_tok :: ShuntingYardParser TermToken
parse_term_tok = do
    val <- TermTok <$> (parse_num <|> parse_char <|> parse_string <|> parse_bool <|> parse_var)
    Parsec.optional type_sig
    return val


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
parse_num = LitInt <$> read <$> Parsec.many1 Parsec.digit

parse_bool :: ShuntingYardParser Term
parse_bool = LitBool <$> read <$> (Parsec.string "True" <|> Parsec.string "False")

parse_var :: ShuntingYardParser Term
parse_var = do
    first <- Parsec.lower <|> Parsec.char '_'
    rest <- Parsec.many (Parsec.lower <|> Parsec.char '_' <|> Parsec.digit)
    return $ Var (Identifier (first:rest))

parse_char :: ShuntingYardParser Term
parse_char = LitChar <$> ((Parsec.char '\'') *> Parsec.anyChar <* (Parsec.char '\''))

parse_string :: ShuntingYardParser Term
parse_string = LitString <$> ((Parsec.char '\"') *> Parsec.many (Parsec.noneOf "\"") <* (Parsec.char '\"'))

parse_left_paren :: ShuntingYardParser TermToken
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> return ()))
    ignore_spaces *> Parsec.char '(' *> return LParen

type_sig :: ShuntingYardParser String
type_sig = do
    Parsec.char ':' *> ignore_spaces
    first <- Parsec.upper
    rest <- Parsec.many (Parsec.lower <|> Parsec.upper <|> Parsec.digit)
    return (first:rest)
