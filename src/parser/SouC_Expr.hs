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
    case runParser (expr <* eof) 0 "expr" input of
        Left err -> putStrLn $ "error: " ++ (show err)
        Right r -> print r

infix_oper_expr = do
    exp1 <- expr
    oper <- try (many1 oper_char)
    exp2 <- expr
    return $ Expr_Infix_Oper exp1 oper exp2


expr :: Parser Expr
expr = souc_char <|> number_lit <|> string_lit <|> prefix_oper <|> (Expr_Identifier <$> identifier) <?> "expression"
--     infix_expr <- infix_oper result
--     return result

oper_char :: Parser Char
oper_char = oneOf "#$%&*+-/<=>?\\^|~"

-- infix_oper :: Parser String
-- infix_oper = many1 oper_char

infix_oper :: Expr -> Parser Expr
infix_oper exp1 = do
    oper <- try (many1 oper_char)
    exp2 <- expr
    return $ Expr_Infix_Oper exp1 oper exp2


postfix_oper :: Parser String
postfix_oper = many1 oper_char

prefix_oper :: Parser Expr
prefix_oper = do
    oper <- string "@" <|> string "!" <|> many1 oper_char
    exp1 <- expr
    return $ Expr_Prefix_Oper oper exp1

number_lit :: Parser Expr
number_lit = Expr_Number <$> ((try octInt <|> try hexInt <|> int) <?> "lit")
    where
        int, octInt, hexInt :: Parser Integer
        octInt = char '0' *> (read . ("0o"++) <$> many1 octDigit)
        hexInt = string "0x" *> (read . ("0x"++) <$> many1 hexDigit)
        int = read <$> (many1 digit)

string_lit :: Parser Expr
string_lit = Expr_StringLit <$> between (char '"') (char '"') (many chars)
    where chars = alphaNum <|> char ' ' -- FIXME many other chars are allowed in strings, just not '"'

souc_char :: Parser Expr
souc_char = do
--     ch <- between (char '\'') (char '\'') anyChar
--     return $ Expr_CharLit ch
    Expr_CharLit <$> between (char '\'') (char '\'') anyChar


