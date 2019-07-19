module SouC_Stmts where

import Debug.Trace

import Text.Parsec hiding (space, spaces, string)

import SouC_Types
import SouC_Expr
import Basics

statement :: Parser Stmt
statement = do
    try indent_depth
    stmt_if <|> stmt_const_assign <|> stmt_var_assign <|> stmt_sub_call <|> stmt_postfix_oper

stmt_block :: Parser Stmts
stmt_block = do
    increase_indent_level
    first_stmt <- statement
    more_stmts <- many (try (endline *> statement))
    decrease_indent_level
    return (first_stmt:more_stmts)

stmt_const_assign :: Parser Stmt
stmt_const_assign = do
    iden <- try (identifier <* spaces <* char '=')
    val <- spaces *> raw_expr
    return $ Stmt_Const_Assign iden val

stmt_var_assign :: Parser Stmt
stmt_var_assign = do
    iden <- try (identifier <* spaces <* string "<-")
    val <- spaces *> raw_expr
    return $ Stmt_Var_Assign iden val

stmt_sub_call :: Parser Stmt
stmt_sub_call = do
    name <- try (identifier <* spaces)
    arg <- optionMaybe raw_expr
    return $ Stmt_Sub_Call name arg

stmt_postfix_oper :: Parser Stmt
stmt_postfix_oper = do
    iden <- identifier
    postfix_op <- postfix_oper
    return $ Stmt_Postfix_Oper iden postfix_op


stmt_if :: Parser Stmt
stmt_if = do
    condition <- try (reserved "if") *> spaces *> raw_expr <* optional_do <* endline
--     thenDo <- many1 (statement <* many1 endline)
    thenDo <- stmt_block
--     elseDo <- optionMaybe (reserved "else" *> endline *> increase_indent_level *> many1 (statement <* many1 endline) <* decrease_indent_level)
    elseDo <- optionMaybe (try (endline *> indent_depth *> reserved "else") *> endline *> stmt_block)
    _ <- optional_end Stmt_If_End -- FIXME use this for type-checking
    return $ Stmt_If condition thenDo elseDo

