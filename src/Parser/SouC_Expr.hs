module Parser.SouC_Expr where

import Debug.Trace

import Text.Parsec hiding (space, spaces, string)
import qualified Data.Map.Strict as Map (Map)

import Common
import Parser.Basics

data Raw_Expr = Raw_Expr String deriving (Read, Show)

run_raw_expr_parser :: String -> String
run_raw_expr_parser input = do
    case runParser (raw_expr <* eof) empty_state "raw_expr" input of
        Left err -> "error: " ++ (show err)
        Right r -> show r

raw_expr :: SouCParser Raw_Expr
raw_expr = Raw_Expr <$> expr_internal

expr_internal :: SouCParser String
expr_internal = (prefix_oper <> keep_spaces <> expr_internal)
    <|> ((string "(" <* skipMany space) <> expr_inside_parens <> (skipMany space *> string ")"))
    <|> (try (term <> keep_spaces <> infix_oper) <> keep_spaces <> expr_internal) -- FIXME whitespace must be equal
    <|> term

expr_inside_parens :: SouCParser String
expr_inside_parens = expr_internal  -- FIXME eventually this must skip newlines

fn_call :: SouCParser String
fn_call = try (raw_identifier <> string "(") <> expr_internal <> string ")"

term :: SouCParser String
term = (fn_call
    <|> raw_identifier
    <|> raw_souc_char
    <|> raw_souc_string
    <|> raw_number_lit) <> option "" type_sig

oper_char :: SouCParser Char
oper_char = oneOf "#$%&*+-/<=>?\\^|~"

prefix_oper :: SouCParser String
prefix_oper = many1 oper_char <|> string "@" <|> string "!"

infix_oper :: SouCParser String
infix_oper = many1 oper_char

postfix_oper :: SouCParser String
postfix_oper = many1 oper_char

raw_number_lit :: SouCParser String
raw_number_lit = hexInt <|> int <?> "lit"
    where
        hexInt = string "0x" *> (("0x"++) <$> many1 hexDigit)
        int = many1 digit

string_char :: SouCParser Char
string_char = noneOf "\""

raw_souc_string :: SouCParser String
raw_souc_string = do
    rest <- (char '"') *> (many string_char) <> (string "\"")
    return $ '"' : rest

raw_souc_char :: SouCParser String
raw_souc_char = do
    ch <- (char '\'') *> anyChar <* (char '\'')
    return $ '\'' : ch : "'"

type_sig :: SouCParser String
type_sig = do
    setup <- (char ':' *> return ":") <> keep_spaces
    first <- upper
    rest <- many (lower <|> upper <|> digit)
    return (setup ++ first : rest)
