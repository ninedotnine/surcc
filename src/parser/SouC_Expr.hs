{-# OPTIONS_GHC -Wall #-}
module SouC_Expr where

import Debug.Trace

-- import Text.Parsec
import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Text.Parsec hiding (space, spaces, string)

import SouC_Types
import Basics


main :: IO ()
main = forever $ do
    input <- putStr "> " >> hFlush stdout >> getLine
    case runParser (raw_expr <* eof) 0 "raw_expr" input of
        Left err -> putStrLn $ "error: " ++ (show err)
        Right r -> print r


raw_expr :: Parser Raw_Expr
raw_expr = Raw_Expr <$> expr_internal

-- expr :: Parser Expr
-- expr = undefined

expr_internal :: Parser String
expr_internal = (prefix_oper <> keep_spaces <> expr_internal)
    <|> (string "(" <> expr_inside_parens <> string ")")
    <|> (try (term <> keep_spaces <> infix_oper) <> keep_spaces <> expr_internal) -- FIXME whitespace must be equal
    <|> term
    <|> (fn_call)

expr_inside_parens :: Parser String
expr_inside_parens = expr_internal  -- FIXME eventually this must skip newlines

fn_call :: Parser String
fn_call = string "IMPOSSIBLE" -- FIXME

term :: Parser String
term = many1 identifier_char <|> raw_souc_char <|> raw_souc_string <|> raw_number_lit

oper_char :: Parser Char
oper_char = oneOf "#$%&*+-/<=>?\\^|~"

prefix_oper :: Parser String
prefix_oper = many1 oper_char <|> string "@" <|> string "!"

infix_oper :: Parser String
infix_oper = many1 oper_char

postfix_oper :: Parser String
postfix_oper = many1 oper_char

number_lit :: Parser Expr
number_lit = Expr_Number <$> ((try octInt <|> try hexInt <|> int) <?> "lit")
    where
        int, octInt, hexInt :: Parser Integer
        octInt = char '0' *> (read . ("0o"++) <$> many1 octDigit)
        hexInt = string "0x" *> (read . ("0x"++) <$> many1 hexDigit)
        int = read <$> (many1 digit)

raw_number_lit :: Parser String
raw_number_lit = ((try octInt <|> try hexInt <|> int) <?> "lit")
    where
        int, octInt, hexInt :: Parser String
        octInt = char '0' *> (("0o"++) <$> many1 octDigit)
        hexInt = string "0x" *> (("0x"++) <$> many1 hexDigit)
        int = (many1 digit)

souc_string :: Parser Expr
souc_string = Expr_StringLit <$> between (char '"') (char '"') (many string_char)

string_char :: Parser Char
string_char = alphaNum <|> char ' ' -- FIXME many other chars are allowed in strings, just not '"'

raw_souc_string :: Parser String
raw_souc_string = do
    rest <- try (char '"') *> (many string_char) <> (string "\"")
    return $ '"' : rest

souc_char :: Parser Expr
souc_char = Expr_CharLit <$> between (char '\'') (char '\'') anyChar

raw_souc_char :: Parser String
raw_souc_char = do
    ch <- try (char '\'') *> anyChar <* (char '\'')
    return $ '\'' : ch : "'"
