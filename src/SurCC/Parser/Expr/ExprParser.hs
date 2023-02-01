-- this is an infix expression parser.
-- it can be extended to support operations with arbitrary precedence.
-- it does not make any attempt at associativity, although this is possible.
-- it gives higher precedence to operators which are not separated by spaces.

module SurCC.Parser.Expr.ExprParser (
    parse_expression,
    parse_raw_expression,
    parse_print_expression,
    evaluate_astree,
    eval_show_astree,
    parse_eval_print_expression,
    ExprTree(..), -- fixme exported by common
    Term(..),
    Operator(..),
    PrefixOperator(..),
) where


import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>), parserFail, ParseError)

-- for trim_spaces
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (dropWhileEnd)

import Data.Char (ord) -- for evaluate

import Data.Text (Text)
import Data.Text qualified as Text
-- import TextShow (TextShow(..))
import TextShow

import SurCC.Parser.Basics (identifier)
import SurCC.Parser.Expr.StackManipulations
import SurCC.Parser.Expr.Types
import SurCC.Parser.Expr.RegardingSpaces
import SurCC.Parser.Expr.Terms
import SurCC.Parser.Expr.Opers
import SurCC.Parser.Expr.Raw  (RawExpr, raw_expr)

import SurCC.Common
import SurCC.Parser.Common (SurCParser)
import SurCC.Common.Parsing (type_name, upper_name, optional_sig, reserved, endline, spaces, string)
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
parse_infix_expression = do
    raw <- raw_expr
    case parse_raw_expression raw of
        Right e -> pure e
        Left err -> parserFail $ "invalid expression:\n" ++ show err


parse_raw_expression :: RawExpr -> Either ParseError ExprTree
parse_raw_expression (RawExpr input) =
    Parsec.runParser parse_term start_state "input" (trim_spaces input)
    where
        start_state = (Oper_Stack [], Tree_Stack [], Tight False)
        trim_spaces = Text.dropWhile isSpace <&> Text.dropWhileEnd isSpace


-- parse_term and parse_oper are alternated
-- until one fails and finish_expr succeeds

parse_term :: ShuntingYardParser ExprTree
parse_term = do
    toke <- parse_term_token
    case toke of
        LParen -> do
            if_tightly_spaced (oper_stack_push StackSpace *> set_spacing_tight False)
            spacing <- Parsec.optionMaybe respect_spaces
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
    toke <- parse_oper_token
    case toke of
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
parse_sig = do
    m_sig <- optional_sig
    case m_sig of
       Just sig -> oper_stack_push (StackSig sig)
       Nothing -> pure ()

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


-- these are little utilities, unrelated to parsing, mostly for testing

parse_print_expression :: Text -> IO ()
parse_print_expression input =
    case parse_raw_expression (RawExpr input) of
        Left err -> putStrLn (show err)
        Right tree -> printT tree

evaluate_astree :: ExprTree -> Integer
evaluate_astree (Leaf t) = case t of
    Lit l -> case l of
        LitInt x -> x
        LitChar c -> fromIntegral (ord c)
        LitString s -> fromIntegral (Text.length s)
    Var _ -> 42 -- all identifiers are bound to this, sure
    Constructor _ -> 43 -- yeah sure no problem here

evaluate_astree (Signed e _) = evaluate_astree e
evaluate_astree (Twig op tree) = operate (evaluate_astree tree)
    where operate = case op of
            Deref -> (\n -> product [1..n]) -- factorial, just for testing
            GetAddr -> undefined
            Negate -> negate
            ToString -> undefined
            Pure -> error $ "can't evaluate pure"
            Join -> error $ "can't evaluate join"

evaluate_astree (Branch op left right) = evaluate_astree left `operate` evaluate_astree right
    where operate = case op of
            Plus   -> (+)
            Minus  -> (-)
            Splat  -> (*)
            FieldDiv -> div -- FIXME
            FloorDiv -> div
            Modulo -> mod
            Hihat  -> (^)
            Equals -> undefined -- can't do this on integers
            GreaterThan -> undefined -- can't do this on integers
            LesserThan -> undefined -- can't do this on integers
            Combine  -> undefined
            Apply  -> undefined -- definitely can't do this
            whatever -> error $ "can't evaluate " ++ show whatever

evaluate_astree (Match _expr _branches) = undefined

eval_show_astree :: ExprTree -> String
eval_show_astree = evaluate_astree <&> show

parse_eval_print_expression :: Text -> IO ()
parse_eval_print_expression input =
    case parse_raw_expression (RawExpr input) of
        Left err -> putStrLn (show err)
        Right tree -> putStrLn (eval_show_astree tree)
