-- oper parsers
module SurCC.Parser.Expr.Opers (
    parse_oper_token
) where

import Data.Foldable (asum)
import Data.Functor ((<&>), void)

import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>))

import SurCC.Parser.Expr.Types
import SurCC.Parser.Expr.RegardingSpaces
import SurCC.Parser.Expr.StackManipulations

parse_oper_token :: ShuntingYardParser OperToken
parse_oper_token =
    (check_for_oper *> apply_tight_prefix_opers *> parse_infix_oper) <|> parse_right_paren
    <?> "infix operator"

check_for_oper :: ShuntingYardParser ()
check_for_oper = void $
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.oneOf valid_op_chars))

valid_op_chars :: String
valid_op_chars = "+-*/%^<>=&~,"

apply_tight_prefix_opers :: ShuntingYardParser ()
apply_tight_prefix_opers = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> pure ()
        (tok:toks) -> case tok of
            StackTightPreOp op -> do
                make_twig op toks
                apply_tight_prefix_opers
            _ -> pure ()


parse_infix_oper :: ShuntingYardParser OperToken
parse_infix_oper = do
    spacing <- Parsec.optionMaybe respect_spaces
    case spacing of
        Nothing -> do
            if_loosely_spaced (oper_stack_push StackSpace)
            set_spacing_tight True
        Just _  -> do
            if_tightly_spaced find_left_space
    oper <- parse_oper_symbol
    if_loosely_spaced (respect_spaces <?> ("space after `" ++ show oper ++ "`"))
    if_tightly_spaced $ no_spaces ("whitespace after `" ++ show oper ++ "`")
    pure (Oper oper)
    where
        str :: String -> ShuntingYardParser String
        str s = Parsec.try ((Parsec.string s) <* Parsec.notFollowedBy (Parsec.oneOf valid_op_chars))

        parse_oper_symbol :: ShuntingYardParser Operator
        parse_oper_symbol = asum (symbols <&> parser) <?> "infix operator"
        parser (chars, op) = str chars *> pure op
        symbols = [
            ("+",   Plus),
            ("-",   Minus),
            ("*",   Splat),
            ("//",  FloorDiv),
            ("/",   FieldDiv),
            ("%",   Modulo),
            ("^",   Hihat ),
            ("<<",  Apply),
            (">>",  FlipApply ),
            ("==",  Equals),
            ("=/=", NotEquals),
            (">",   GreaterThan),
            ("<",   LesserThan),
            ("=~",  RegexMatch),
            ("<>",  Combine),
            ("AND", And),
            ("OR",  Or),
            ("><",  Xor),
            ("IN",  In),
            (",",   Comma),
            ("?",   Iff),
            ("??",  FromMaybe),
            (">|",  Prepend),
            ("|<",  Append),
            ("#",   Index),
            ("##",  Lookup),
            ("<~&>", Map),
            ("<&>", FlipMap),
            ("<~*>", Applicative),
            ("<*>", FlipApplicative),
            ("*>",  SequenceRight),
            ("<*",  SequenceLeft),
            (">>=", BindRight),
            ("=<<", BindLeft)
            ]


parse_right_paren :: ShuntingYardParser OperToken
parse_right_paren = do
    spacing <- Parsec.optionMaybe respect_spaces
    _ <- Parsec.char ')' <?> ""
    pure $ case spacing of
        Nothing -> RParen
        Just () -> RParenAfterSpace
