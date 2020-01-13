module Parser.SouC_Expr where

import Debug.Trace

import Text.Parsec hiding (space, spaces, string)
import qualified Data.Map.Strict as Map (Map)

import Common
import Parser.Basics

run_raw_expr_parser :: String -> String
run_raw_expr_parser input = do
    case runParser (raw_expr <* eof) empty_state "raw_expr" input of
        Left err -> "error: " ++ (show err)
        Right r -> show r

raw_expr :: SouCParser Raw_Expr
raw_expr = Raw_Expr <$> expr_internal

-- expr :: SouCParser Expr
-- expr = undefined

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
term = fn_call
    <|> raw_identifier
    <|> raw_souc_char
    <|> raw_souc_string
    <|> raw_number_lit

oper_char :: SouCParser Char
oper_char = oneOf "#$%&*+-/<=>?\\^|~"

prefix_oper :: SouCParser String
prefix_oper = many1 oper_char <|> string "@" <|> string "!"

infix_oper :: SouCParser String
infix_oper = many1 oper_char

postfix_oper :: SouCParser String
postfix_oper = many1 oper_char

number_lit :: SouCParser Expr
number_lit = Expr_Number <$> ((try octInt <|> try hexInt <|> int) <?> "lit")
    where
        int, octInt, hexInt :: SouCParser Integer
        octInt = char '0' *> (read . ("0o"++) <$> many1 octDigit)
        hexInt = string "0x" *> (read . ("0x"++) <$> many1 hexDigit)
        int = read <$> (many1 digit)

raw_number_lit :: SouCParser String
raw_number_lit = ((try octInt <|> try hexInt <|> int) <?> "lit")
    where
        int, octInt, hexInt :: SouCParser String
        octInt = char '0' *> (("0o"++) <$> many1 octDigit)
        hexInt = string "0x" *> (("0x"++) <$> many1 hexDigit)
        int = (many1 digit)

souc_string :: SouCParser Expr
souc_string = Expr_StringLit <$> between (char '"') (char '"') (many (noneOf "\""))

string_char :: SouCParser Char
string_char = alphaNum <|> char ' ' -- FIXME many other chars are allowed in strings, just not '"'

raw_souc_string :: SouCParser String
raw_souc_string = do
    rest <- try (char '"') *> (many string_char) <> (string "\"")
    return $ '"' : rest

souc_char :: SouCParser Expr
souc_char = Expr_CharLit <$> between (char '\'') (char '\'') anyChar

raw_souc_char :: SouCParser String
raw_souc_char = do
    ch <- try (char '\'') *> anyChar <* (char '\'')
    return $ '\'' : ch : "'"
