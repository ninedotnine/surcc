-- this is an infix expression parser.
-- it can be extended to support operations with arbitrary precedence.
-- it does not make any attempt at associativity, although this is possible.
-- it gives higher precedence to operators which are not separated by spaces.

module SurCC.Parser.Expr.ExprParser (
    parse_expression,
    parse_text_as_expr,
    ExprTree(..), -- fixme exported by common
    Term(..),
    Operator(..),
    PrefixOperator(..),
) where


import Data.Char (isSpace) -- for trim_spaces

import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Function

import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>), parserFail, ParseError)

import SurCC.Parser.Expr.StackManipulations
import SurCC.Parser.Expr.Types
import SurCC.Parser.Expr.RegardingSpaces
import SurCC.Parser.Expr.Terms
import SurCC.Parser.Expr.Opers
import SurCC.Parser.Expr.Raw (raw_expr)

import SurCC.Common
import SurCC.Parser.Common (SurCParser)
import SurCC.Common.Parsing (
    optional_sig,
    reserved,
    endline,
    spaces,
    ignore_spaces,
    string
    )
import SurCC.Common.TextShow ()
import SurCC.Parser.Patterns  (parse_pattern)


parse_expression :: SurCParser ExprTree
parse_expression = parse_match <|> parse_infix_expression

-- parse_match cannot be followed by anything else

parse_match :: SurCParser ExprTree
parse_match = do
    reserved "match" *> spaces
    scrutinee <- parse_infix_expression
    endline
    (i, _) <- Parsec.getState
    let indent = Parsec.count (i+1) Parsec.tab
    cases <- Parsec.many1 (indent *> parse_match_case)
    pure $ Match scrutinee cases
    where
        parse_match_case :: SurCParser (Pattern, Maybe Guard, ExprTree)
        parse_match_case = do
            pat <- parse_pattern
            spaces
            guard <- Parsec.optionMaybe parse_guard
            string "->" *> spaces
            expr <- parse_expression
            endline
            pure (pat, guard, expr)
        parse_guard :: SurCParser Guard
        parse_guard = do
            Parsec.try (reserved "if")
            raw <- Parsec.manyTill Parsec.anyChar
                                   (Parsec.lookAhead (string " ->"))
            spaces
            case parse_raw_expression (RawExpr (Text.pack raw)) of
                Right expr -> pure (Guard expr)
                Left err -> parserFail $
                    "invalid pattern guard:\n" ++ show err


parse_infix_expression :: Parsec.Parsec Text s ExprTree
parse_infix_expression = raw_expr <&> parse_raw_expression >>= \case
    Right e -> pure e
    Left err -> parserFail $ "invalid expression: " ++ show expr_str
        where expr_str = err & Parsec.errorPos & Parsec.sourceName


parse_raw_expression :: RawExpr -> Either ParseError ExprTree
parse_raw_expression (RawExpr input) =
    Parsec.runParser parse_term start_state (Text.unpack input) (trim_spaces input)
    where
        start_state = (Oper_Stack [], Tree_Stack [], Tight False)
        trim_spaces = Text.dropWhile isSpace <&> Text.dropWhileEnd isSpace


parse_text_as_expr :: Text -> Either ParseError ExprTree
parse_text_as_expr = parse_raw_expression . RawExpr

-- parse_term and parse_oper are alternated
-- until one fails and finish_expr succeeds

parse_term :: ShuntingYardParser ExprTree
parse_term = do
    parse_term_token >>= \case
        LParen -> do
            if_tightly_spaced (oper_stack_push StackSpace *> set_spacing_tight False)
            spacing <- Parsec.optionMaybe spaces
            case spacing of
                Nothing -> oper_stack_push StackLParen
                Just () -> oper_stack_push StackLParenFollowedBySpace
            parse_term
        TermTok t -> do
            m_sig <- optional_sig
            case m_sig of
                Nothing -> tree_stack_push (Leaf t)
                Just sig -> tree_stack_push (Signed (Leaf t) sig)
            parse_oper <|> finish_expr
        TightPreOp op -> do
            oper_stack_push (StackTightPreOp op)
            parse_term
        SpacedPreOp op -> do
            oper_stack_push (StackSpacedPreOp op)
            parse_term

parse_oper :: ShuntingYardParser ExprTree
parse_oper = do
    parse_oper_token >>= \case
        RParen -> do
            if_tightly_spaced find_left_space
            look_for StackLParen
            Oper_Stack stack_ops <- get_op_stack
            parse_sig
            case stack_ops of
                (StackSpace:ops) -> oper_stack_set ops *> set_spacing_tight True
                _ -> pure ()
            parse_oper <|> finish_expr
        RParenAfterSpace -> do
            if_tightly_spaced find_left_space
            look_for StackLParenFollowedBySpace
            Oper_Stack stack_ops <- get_op_stack
            parse_sig
            case stack_ops of
                (StackSpace:ops) -> oper_stack_set ops *> set_spacing_tight True
                _ -> pure ()
            parse_oper <|> finish_expr
        Oper op -> do
            apply_higher_prec_ops (get_prec op)
            oper_stack_push (StackOp op)
            parse_term

parse_sig :: ShuntingYardParser ()
parse_sig = optional_sig >>= traverse_ (oper_stack_push . StackSig)

finish_expr :: ShuntingYardParser ExprTree
finish_expr = do
    parse_sig
    ignore_spaces
-- FIXME remove this line
--     Parsec.optional Parsec.newline <?> ""
    Parsec.eof <?> ""
    clean_stack
    Tree_Stack tree <- get_tree_stack
    case tree of
        [] -> Parsec.parserFail "bad expression"
        (result:[]) -> pure result
        _ -> Parsec.parserFail "invalid expression, something is wrong here."
