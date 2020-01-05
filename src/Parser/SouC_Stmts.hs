module SouC_Stmts where

import Debug.Trace

import Text.Parsec hiding (space, spaces, string)

import SouC_Types
import SouC_Expr
import Basics
import ExprParser

statement :: Parser Stmt
statement = do
    try indent_depth
    stmt_cond <|> stmt_loop <|> stmt_return <|> stmt_beginning_with_identifier


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
    return $ Stmts (first_stmt:more_stmts)

stmt_const_assign :: Identifier -> Parser Stmt
stmt_const_assign name = do
    _ <- try (spaces <* char '=')
    Raw_Expr val <- spaces *> raw_expr
    case parse_expression val of
        Right expr -> do
            add_to_bindings name expr
            return $ Stmt_Const_Assign name expr
        Left err -> parserFail $ "invalid expression:\n" ++ show err

stmt_var_assign :: Identifier -> Parser Stmt
stmt_var_assign name = do
    _ <- try (spaces <* string "<-")
    Raw_Expr val <- spaces *> raw_expr
    case parse_expression val of
        Right expr -> return $ Stmt_Var_Assign name expr
        Left err -> parserFail $ "invalid expression:\n" ++ show err

stmt_sub_call :: Identifier -> Parser Stmt
stmt_sub_call name = do
    m_arg <- optionMaybe (try (spaces *> raw_expr))
    case m_arg of
        Nothing -> return $ Stmt_Sub_Call name Nothing
        Just (Raw_Expr arg) -> case parse_expression arg of
            Left err -> parserFail $ "invalid expression:\n" ++ show err
            Right expr -> return $ Stmt_Sub_Call name (Just expr)

stmt_postfix_oper :: Identifier -> Parser Stmt
stmt_postfix_oper name = do
    postfix_op <- postfix_oper
    return $ Stmt_Postfix_Oper name postfix_op

stmt_loop :: Parser Stmt
stmt_loop = stmt_while <|> stmt_until

stmt_while :: Parser Stmt
stmt_while = do
    Raw_Expr condition <- try (reserved "while") *> spaces *> raw_expr <* optional_do <* endline
    stmts <- stmt_block
    _ <- optional_end Stmt_While_End -- FIXME use this for type-checking
    case parse_expression condition of
        Right expr -> return $ Stmt_While expr stmts
        Left err -> parserFail $ "invalid expression:\n" ++ show err

stmt_until :: Parser Stmt
stmt_until = do
    Raw_Expr condition <- try (reserved "until") *> spaces *> raw_expr <* optional_do <* endline
    stmts <- stmt_block
    _ <- optional_end Stmt_Until_End -- FIXME use this for type-checking
    case parse_expression condition of
        Right expr -> return $ Stmt_Until expr stmts
        Left err -> parserFail $ "invalid expression:\n" ++ show err

stmt_cond :: Parser Stmt
stmt_cond = stmt_if <|> stmt_unless

stmt_if :: Parser Stmt
stmt_if = do
    Raw_Expr condition <- try (reserved "if") *> spaces *> raw_expr <* optional_do <* endline
    thenDo <- stmt_block
    elseDo <- optionMaybe (try (endline *> indent_depth *> reserved "else") *> endline *> stmt_block)
    _ <- optional_end Stmt_If_End -- FIXME use this for type-checking
    case parse_expression condition of
        Right tree -> return $ Stmt_If tree thenDo elseDo
        Left err -> parserFail $ "failed expression:\n" ++ show err

stmt_unless :: Parser Stmt
stmt_unless = do
    Raw_Expr condition <- try (reserved "unless") *> spaces *> raw_expr <* optional_do <* endline
    thenDo <- stmt_block
    elseDo <- optionMaybe (try (endline *> indent_depth *> reserved "else") *> endline *> stmt_block)
    _ <- optional_end Stmt_Unless_End -- FIXME use this for type-checking
    case parse_expression condition of
        Right tree -> return $ Stmt_Unless tree thenDo elseDo
        Left err -> parserFail $ "failed expression:\n" ++ show err

stmt_return :: Parser Stmt
stmt_return = do
    result <- reserved "return" *> spaces *> optionMaybe raw_expr
    return (Stmt_Return result)
