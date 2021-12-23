-- this is an infix expression parser.
-- it can be extended to support operations with arbitrary precedence.
-- it does not make any attempt at associativity, although this is possible.
-- it gives higher precedence to operators which are not separated by spaces.

module Parser.Expr.ExprParser (
    parse_expression,
    parse_print_expression,
    evaluate_astree,
    eval_show_astree,
    parse_eval_print_expression,
    ExprTree(..),
    Term(..),
    Operator(..),
    PrefixOperator(..),
    RawExpr(..),
) where


import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>))

-- for trim_spaces
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (dropWhileEnd)

import Data.Char (ord) -- for evaluate

import Data.Text (Text)
import Data.Text qualified as Text
-- import TextShow (TextShow(..))
import TextShow

import Parser.Expr.StackManipulations
import Parser.Expr.Types
import Parser.Expr.RegardingSpaces
import Parser.Expr.Terms
import Parser.Expr.Opers
import Parser.Expr.Raw  (RawExpr)

import Common
import Common.Parsing (type_name, upper_name, optional_sig)

-- parse_term and parse_oper are alternated until one fails and finish_expr succeeds
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
    Parsec.optional Parsec.newline <?> ""
    Parsec.eof <?> ""
    clean_stack
    Tree_Stack tree <- get_tree_stack
    case tree of
        [] -> Parsec.parserFail "bad expression"
        (result:[]) -> pure result
        _ -> Parsec.parserFail "invalid expression, something is wrong here."


parse_expression :: RawExpr -> Either Parsec.ParseError ExprTree
parse_expression (RawExpr input) =
    Parsec.runParser parse_term start_state "input" (trim_spaces input)
    where
        start_state = (Oper_Stack [], Tree_Stack [], Tight False)
        trim_spaces = Text.dropWhile isSpace <&> Text.dropWhileEnd isSpace


-- these are little utilities, unrelated to parsing, mostly for testing

parse_print_expression :: Text -> IO ()
parse_print_expression input = case parse_expression (RawExpr input) of
        Left err -> putStrLn (show err)
        Right tree -> printT tree

evaluate_astree :: ExprTree -> Integer
evaluate_astree (Leaf t) = case t of
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


eval_show_astree :: ExprTree -> String
eval_show_astree = evaluate_astree <&> show

parse_eval_print_expression :: Text -> IO ()
parse_eval_print_expression input = case parse_expression (RawExpr input) of
    Left err -> putStrLn (show err)
    Right tree -> putStrLn (eval_show_astree tree)
