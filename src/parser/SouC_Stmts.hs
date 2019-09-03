module SouC_Stmts where

import Debug.Trace

import Text.Parsec hiding (space, spaces, string)

import SouC_Types
import SouC_Expr
import Basics

statement :: Parser Stmt
statement = do
    try indent_depth
    stmt_if <|> stmt_while <|> stmt_return <|> stmt_beginning_with_identifier


stmt_beginning_with_identifier :: Parser Stmt
stmt_beginning_with_identifier = do
    iden <- identifier
    return =<< (stmt_const_assign iden <|>
                stmt_var_assign iden <|>
                stmt_postfix_oper iden <|>
                stmt_sub_call iden)

stmt_block :: Parser Stmts
stmt_block = do
    increase_indent_level
    first_stmt <- statement
    more_stmts <- many (try (endline *> statement))
    decrease_indent_level
    return (first_stmt:more_stmts)

stmt_const_assign :: Identifier -> Parser Stmt
stmt_const_assign name = do
    _ <- try (spaces <* char '=')
    val <- spaces *> raw_expr
    return $ Stmt_Const_Assign name val

stmt_var_assign :: Identifier -> Parser Stmt
stmt_var_assign name = do
    _ <- try (spaces <* string "<-")
    val <- spaces *> raw_expr
    return $ Stmt_Var_Assign name val

stmt_sub_call :: Identifier -> Parser Stmt
stmt_sub_call name = do
    m_arg <- optionMaybe (try (spaces *> raw_expr))
    return $ Stmt_Sub_Call name m_arg

stmt_postfix_oper :: Identifier -> Parser Stmt
stmt_postfix_oper name = do
    postfix_op <- postfix_oper
    return $ Stmt_Postfix_Oper name postfix_op

stmt_while :: Parser Stmt
stmt_while = do
    condition <- try (reserved "while") *> spaces *> raw_expr <* optional_do <* endline
    stmts <- stmt_block
    _ <- optional_end Stmt_While_End -- FIXME use this for type-checking
    return $ Stmt_While condition stmts

stmt_if :: Parser Stmt
stmt_if = do
    condition <- try (reserved "if") *> spaces *> raw_expr <* optional_do <* endline
--     thenDo <- many1 (statement <* many1 endline)
    thenDo <- stmt_block
--     elseDo <- optionMaybe (reserved "else" *> endline *> increase_indent_level *> many1 (statement <* many1 endline) <* decrease_indent_level)
    elseDo <- optionMaybe (try (endline *> indent_depth *> reserved "else") *> endline *> stmt_block)
    _ <- optional_end Stmt_If_End -- FIXME use this for type-checking
    return $ Stmt_If condition thenDo elseDo

stmt_return :: Parser Stmt
stmt_return = do
    result <- reserved "return" *> spaces *> optionMaybe raw_expr
    return (Stmt_Return result)
