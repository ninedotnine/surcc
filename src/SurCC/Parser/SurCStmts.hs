module SurCC.Parser.SurCStmts (
    stmt_block,
    stmt_block_with_param
) where

import Data.Map.Strict qualified as Map (empty, singleton, member, lookup)
import Data.List.NonEmpty ( NonEmpty(..), cons )
import Text.Parsec hiding (space, spaces, string)

import SurCC.Common
import SurCC.Parser.Common
import SurCC.Common.Parsing
import SurCC.Parser.Expr.Raw
import SurCC.Parser.Basics
import SurCC.Parser.ExprParser

stmt_block :: SurCParser Stmts
stmt_block = stmt_block_with_param Nothing

stmt_block_with_param :: Maybe Param -> SurCParser Stmts
stmt_block_with_param p = do
    increase_indent_level
    add_param_to_bindings p
    statements <- many1 (stmt <* many endline)
    decrease_indent_level
    pure $ Stmts statements
        where
            increase_indent_level :: SurCParser ()
            increase_indent_level = modifyState (\(x,m) -> (x+1, cons Map.empty m))
            decrease_indent_level :: SurCParser ()
            decrease_indent_level = modifyState dedent
                where
                    dedent = \case
                        (x, _ :| (m:ms)) -> (x-1, m:|ms)
                        (_, _) -> error "should be impossible"
                                    -- consider a tagged type

add_param_to_bindings :: Maybe Param -> SurCParser ()
add_param_to_bindings = \case
    Just (Param p _) -> add_to_bindings p Immut
    _ -> pure ()


-- maybe i will find a use for these?
new_scope :: SurCParser ()
new_scope = modifyState (\(x,m) -> (x, cons Map.empty m))

end_scope :: SurCParser ()
end_scope = modifyState dedent
    where
        dedent = \case
            (x, _ :| (m:ms)) -> (x, m:|ms)
            (_, _) -> error "should be impossible"


stmt :: SurCParser Stmt
stmt = do
    try (many endline *> indentation)
    stmt_cond <|> stmt_loop <|> stmt_return <|> stmt_beginning_with_identifier

stmt_beginning_with_identifier :: SurCParser Stmt
stmt_beginning_with_identifier = do
    iden <- identifier
    pure =<< (stmt_const_assign iden <|>
                stmt_var_assign iden <|>
                stmt_postfix_oper iden <|>
                stmt_sub_call iden)


stmt_const_assign :: Identifier -> SurCParser Stmt
stmt_const_assign name = do
    sig <- try (optional_sig <* spaces <* char '=')
    add_to_bindings name Immut
    val <- spaces *> raw_expr
    expr <- case parse_expression val of
        Right e -> pure $ Stmt_Const_Assign name sig e
        Left err -> parserFail $ "invalid expression:\n" ++ show err
    endline
    pure expr


stmt_var_assign :: Identifier -> SurCParser Stmt
stmt_var_assign name = do
    m_sig <- try (optional_sig <* spaces <* string "<-")
    ass_or_reass <- bindings_lookup name >>= \case
        Just Mut -> pure $ Stmt_Var_Reassign name
        Just Immut -> parserFail $ "tried to reassign: " ++ show name
        Nothing -> do
            add_to_bindings name Mut
            pure $ Stmt_Var_Assign name m_sig
    val <- spaces *> raw_expr
    expr <- case parse_expression val of
        Right e -> pure e
        Left err -> parserFail $ "invalid expression:\n" ++ show err
    endline
    pure (ass_or_reass expr)

stmt_sub_call :: Identifier -> SurCParser Stmt
stmt_sub_call name = do
    m_arg <- optionMaybe (try (spaces *> raw_expr))
    endline
    case m_arg of
        Nothing -> pure $ Stmt_Sub_Call name Nothing
        Just arg -> case parse_expression arg of
            Left err -> parserFail $ "invalid expression:\n" ++ show err
            Right expr -> pure $ Stmt_Sub_Call name (Just expr)

stmt_postfix_oper :: Identifier -> SurCParser Stmt
stmt_postfix_oper name = do
    postfix_op <- postfix_oper
    pure $ Stmt_Postfix_Oper name postfix_op

stmt_loop :: SurCParser Stmt
stmt_loop = stmt_while <|> stmt_until

stmt_while :: SurCParser Stmt
stmt_while = do
    condition <- try (reserved "while") *> spaces *> raw_expr
    stmts <- optional_do *> endline *> stmt_block
    optional (many endline *> end_block Stmt_While_End) -- FIXME use this for type-checking
    case parse_expression condition of
        Right expr -> pure $ Stmt_While expr stmts
        Left err -> parserFail $ "invalid expression:\n" ++ show err

stmt_until :: SurCParser Stmt
stmt_until = do
    condition <- try (reserved "until") *> spaces *> raw_expr
    stmts <- optional_do *> endline *> stmt_block
    optional (end_block Stmt_Until_End) -- FIXME use this for type-checking
    case parse_expression condition of
        Right expr -> pure $ Stmt_Until expr stmts
        Left err -> parserFail $ "invalid expression:\n" ++ show err

stmt_cond :: SurCParser Stmt
stmt_cond = stmt_if <|> stmt_unless

stmt_if :: SurCParser Stmt
stmt_if = do
    condition <- try (reserved "if") *> spaces *> raw_expr
    thenDo <- optional_do *> endline *> stmt_block
    elseDo <- optionMaybe ((try (indentation *> reserved "else"))
                                *> endline
                                *> stmt_block)
    optional (end_block Stmt_If_End) -- FIXME use this for type-checking
    case parse_expression condition of
        Right tree -> pure $ Stmt_If tree thenDo elseDo
        Left err -> parserFail $ "failed expression:\n" ++ show err

stmt_unless :: SurCParser Stmt
stmt_unless = do
    condition <- try (reserved "unless") *> spaces *> raw_expr
    thenDo <- optional_do *> endline *> stmt_block
    elseDo <- optionMaybe ((try (indentation *> reserved "else"))
                                *> endline
                                *> stmt_block)
    optional (end_block Stmt_Unless_End) -- FIXME use this for type-checking
    case parse_expression condition of
        Right tree -> pure $ Stmt_Unless tree thenDo elseDo
        Left err -> parserFail $ "failed expression:\n" ++ show err

stmt_return :: SurCParser Stmt
stmt_return = do
    result <- reserved "return" *> optionMaybe (try (spaces *> raw_expr))
    case result of
        Nothing -> pure (Stmt_Return Nothing)
        Just raw_exp -> case parse_expression raw_exp of
            Right expr -> pure (Stmt_Return (Just expr))
            Left err -> parserFail $ "failed expression:\n" ++ show err

