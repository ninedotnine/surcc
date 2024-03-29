module SurCC.Parser.SurCStmts (
    stmt_block,
    stmt_block_with_param
) where

import Data.Foldable (for_)
import Data.Map.Strict qualified as Map (empty, singleton, member, lookup)
import Data.List.NonEmpty ( NonEmpty(..), cons )
import Text.Parsec hiding (space, spaces, string)

import SurCC.Common
import SurCC.Parser.Common
import SurCC.Common.Parsing
import SurCC.Parser.Expr.Raw (postfix_oper) -- fixme delete this?
import SurCC.Parser.Basics
import SurCC.Parser.ExprParser (parse_expression)


-- FIXME -- not all blocks are the same:
-- if blocks can be only a return statement
-- while blocks cannot have returns at all


stmt_block :: SurCParser Stmts
stmt_block = stmt_block_with_param Nothing


stmt_block_with_param :: Maybe Param -> SurCParser Stmts
stmt_block_with_param p = do
    new_scope
    for_ p add_param_to_bindings
    statements <- many1 (stmt <* many endline)
    ret <- optionMaybe (stmt_return <* endline)
    end_scope
    pure $ Stmts statements ret


stmt_block_if :: SurCParser Stmts
stmt_block_if = do
    new_scope
    immediate_ret <- optionMaybe (stmt_return <* endline)
    block <- case immediate_ret of
        Just _ -> pure $ Stmts [] immediate_ret
        Nothing -> do
            statements <- many1 (stmt <* many endline)
            ret <- optionMaybe (stmt_return <* endline)
            pure $ Stmts statements ret
    end_scope
    pure block


stmt_block_while :: SurCParser Stmts
stmt_block_while = do
    new_scope
    statements <- many1 (stmt <* many endline)
    end_scope
    pure $ Stmts statements Nothing




new_scope :: SurCParser ()
new_scope = modifyState (\(x,m) -> (x+1, cons Map.empty m))

end_scope :: SurCParser ()
end_scope = modifyState dedent
    where
        dedent = \case
            (x, _ :| (m:ms)) -> (x-1, m:|ms)
            (_, _) -> error "should be impossible"
                        -- consider a tagged type


add_param_to_bindings :: Param -> SurCParser ()
add_param_to_bindings (Param p _) = add_to_bindings p Immut



stmt :: SurCParser Stmt
stmt = do
    try (many endline *> indentation *> notFollowedBy (reserved "return"))
    stmt_control_flow <|> stmt_declaration <|> stmt_beginning_with_identifier

stmt_control_flow :: SurCParser Stmt
stmt_control_flow = stmt_cond <|> stmt_loop

stmt_declaration :: SurCParser Stmt
stmt_declaration = stmt_declare_dynamic_const <|> stmt_declare_var

stmt_declare_dynamic_const :: SurCParser Stmt
stmt_declare_dynamic_const = do
    name <- reserved "let" *> spaces *> identifier
    sig <- optional_sig <* spaces <* char '='
    add_to_bindings name Immut
    expr <- spaces *> parse_expression
    endline
    pure $ Stmt_Const_Assign_Dynamic name sig expr

stmt_declare_var :: SurCParser Stmt
stmt_declare_var = do
    name <- reserved "var" *> spaces *> identifier
    m_sig <- optional_sig <* spaces <* string "<-"
    add_to_bindings name Mut
    expr <- spaces *> parse_expression
    endline
    pure $ Stmt_Var_Declare name m_sig expr


stmt_beginning_with_identifier :: SurCParser Stmt
stmt_beginning_with_identifier = do
    iden <- identifier
    pure =<< (stmt_const_assign iden <|>
              stmt_var_reassign iden <|>
                stmt_postfix_oper iden <|>
                stmt_sub_call iden)


stmt_const_assign :: Identifier -> SurCParser Stmt
stmt_const_assign name = do
    sig <- try (optional_sig <* spaces <* char '=')
    add_to_bindings name Immut
    expr <- spaces *> parse_expression
    endline
    pure $ Stmt_Const_Assign_Static name sig expr


stmt_var_reassign :: Identifier -> SurCParser Stmt
stmt_var_reassign name = do
    m_sig <- try (optional_sig <* spaces <* string "<-")
    bindings_lookup name >>= \case
        Just Mut -> pure ()
        Just Immut -> parserFail $ "tried to reassign const: " ++ show name
        Nothing -> parserFail $ "tried to reassign undeclared: " ++ show name
    expr <- spaces *> parse_expression
    endline
    pure $ Stmt_Var_Reassign name m_sig expr

stmt_sub_call :: Identifier -> SurCParser Stmt
stmt_sub_call name = do
    m_arg <- optionMaybe $ try (spaces *> lookAhead expr_start)
                           *> parse_expression
    endline
    pure $ Stmt_Sub_Call name m_arg
    where
        expr_start = identifier_char <|> oneOf (op_chars <> "([{\"\'")


stmt_postfix_oper :: Identifier -> SurCParser Stmt
stmt_postfix_oper name = do
    postfix_op <- postfix_oper
    pure $ Stmt_Postfix_Oper name postfix_op

stmt_loop :: SurCParser Stmt
stmt_loop = stmt_while <|> stmt_until

stmt_while :: SurCParser Stmt
stmt_while = do
    condition <- reserved "while" *> spaces *> parse_expression
    stmts <- optional_do *> endline *> stmt_block_while
    optional (many endline *> end_block Stmt_While_End) -- FIXME use this for type-checking
    pure $ Stmt_While condition stmts

stmt_until :: SurCParser Stmt
stmt_until = do
    condition <- reserved "until" *> spaces *> parse_expression
    stmts <- optional_do *> endline *> stmt_block_while
    optional (end_block Stmt_Until_End) -- FIXME use this for type-checking
    pure $ Stmt_Until condition stmts

stmt_cond :: SurCParser Stmt
stmt_cond = stmt_if <|> stmt_unless

stmt_if :: SurCParser Stmt
stmt_if = do
    condition <- reserved "if" *> spaces *> parse_expression
    thenDo <- optional_do *> endline *> stmt_block_if
    elseDo <- optionMaybe ((try (indentation *> reserved' "else"))
                                *> endline
                                *> stmt_block_if)
    optional (end_block Stmt_If_End) -- FIXME use this for type-checking
    pure $ Stmt_If condition thenDo elseDo

stmt_unless :: SurCParser Stmt
stmt_unless = do
    condition <- reserved "unless" *> spaces *> parse_expression
    thenDo <- optional_do *> endline *> stmt_block_if
    elseDo <- optionMaybe ((try (indentation *> reserved' "else"))
                                *> endline
                                *> stmt_block_if)
    optional (end_block Stmt_Unless_End) -- FIXME use this for type-checking
    pure $ Stmt_Unless condition thenDo elseDo

stmt_return :: SurCParser Return
stmt_return = do
    try (many endline *> indentation *> reserved "return")
    m_expr <- optionMaybe (try (spaces *> parse_expression))
    pure $ Return m_expr
