-- this is an infix expression parser.
-- it can be extended to support operations with arbitrary precedence.
-- it does not make any attempt at associativity, although this is possible.
-- it gives higher precedence to operators which are not separated by spaces.

module Parser.Expr.ExprParser (
    pretty_show_expression,
    parse_expression,
    parse_print_expression,
    evaluate_astree,
    eval_show_astree,
    parse_eval_print_expression,
    ASTree(..),
    Term(..),
    Operator(..),
    PrefixOperator(..)
) where


import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), (<?>))

-- for trim_spaces
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (dropWhileEnd)

import Data.Char (ord) -- for evaluate

import Parser.Expr.StackManipulations
import Parser.Expr.ExprTypes
import Parser.Expr.RegardingSpaces
import Parser.Expr.Terms
import Parser.Expr.Opers

import Common

-- parse_term and parse_oper are alternated until one fails and finish_expr succeeds
parse_term :: ShuntingYardParser ASTree
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

parse_oper :: ShuntingYardParser ASTree
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

finish_expr :: ShuntingYardParser ASTree
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


-- these are little utilities, unrelated to parsing
pretty_show_expression :: ASTree -> String
pretty_show_expression (Branch oper left right) = "(" ++ show oper ++ " "  ++ pretty_show_expression left ++ " " ++ pretty_show_expression right ++ ")"
pretty_show_expression (Twig oper tree) = concat ["(", show oper, " ", pretty_show_expression tree, ")"]
pretty_show_expression (Signed tree tree_t) = pretty_show_expression tree ++ ": " ++ show tree_t
pretty_show_expression (Leaf val) = show val

parse_expression :: String -> Either Parsec.ParseError ASTree
parse_expression input = Parsec.runParser parse_term start_state "input" (trim_spaces input)
    where
        start_state = (Oper_Stack [], Tree_Stack [], Tight False)
        trim_spaces = dropWhile isSpace <&> dropWhileEnd isSpace


optional_sig :: ShuntingYardParser (Maybe SoucType)
optional_sig = Parsec.optionMaybe type_sig where
    type_sig :: ShuntingYardParser SoucType
    type_sig = do
        Parsec.char ':' *> ignore_spaces
        first <- Parsec.upper
        rest <- Parsec.many (Parsec.lower <|> Parsec.upper <|> Parsec.digit)
        pure (SoucType (first:rest))


parse_print_expression :: String -> IO ()
parse_print_expression input = case parse_expression input of
        Left err -> putStrLn (show err)
        Right tree -> putStrLn (pretty_show_expression tree)

evaluate_astree :: ASTree -> Integer
evaluate_astree (Leaf t) = case t of
    LitInt x -> x
    LitChar c -> fromIntegral (ord c)
    LitBool b -> if b then 1 else 0
    LitString s -> fromIntegral (length s)
    Var _ -> 42 -- all identifiers are bound to this, sure
evaluate_astree (Signed e _) = evaluate_astree e
evaluate_astree (Twig op tree) = operate (evaluate_astree tree)
    where operate = case op of
            Deref -> (\n -> product [1..n]) -- factorial, just for testing
            GetAddr -> undefined
            Negate -> negate
            ToString -> undefined
            whatever -> error $ "can't evaluate" ++ show whatever
evaluate_astree (Branch op left right) = evaluate_astree left `operate` evaluate_astree right
    where operate = case op of
            Plus   -> (+)
            Minus  -> (-)
            Splat  -> (*)
            Divide -> div
            Modulo -> mod
            Hihat  -> (^)
            Equals -> undefined -- can't do this on integers
            GreaterThan -> undefined -- can't do this on integers
            LesserThan -> undefined -- can't do this on integers
            Combine  -> undefined
            Apply  -> undefined -- definitely can't do this
            whatever -> error $ "can't evaluate " ++ show whatever


eval_show_astree :: ASTree -> String
eval_show_astree = evaluate_astree <&> show

parse_eval_print_expression :: String -> IO ()
parse_eval_print_expression input = case parse_expression input of
    Left err -> putStrLn (show err)
    Right tree -> putStrLn (eval_show_astree tree)
