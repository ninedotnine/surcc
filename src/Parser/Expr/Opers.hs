-- oper parsers
module Expr.Opers (
    parse_oper_token
) where

import qualified Text.Parsec as Parsec
import Text.Parsec ((<|>), (<?>))

import Expr.ExprTypes
import Expr.RegardingSpaces
import Expr.StackManipulations

parse_oper_token :: ShuntingYardParser OperToken
parse_oper_token =
    (check_for_oper *> apply_tight_prefix_opers *> parse_infix_oper) <|> parse_right_paren
    <?> "infix operator"

check_for_oper :: ShuntingYardParser ()
check_for_oper = Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.oneOf valid_op_chars)) *> return ()

valid_op_chars :: String
valid_op_chars = "+-*/%^<>=&"

apply_tight_prefix_opers :: ShuntingYardParser ()
apply_tight_prefix_opers = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        (tok:toks) -> case tok of
            StackTightPreOp op -> do
                make_twig op toks
                apply_tight_prefix_opers
            _ -> return ()


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
    return (Oper oper)
    where
        str :: String -> ShuntingYardParser String
        char :: Char -> ShuntingYardParser Char
        str s = Parsec.try (Parsec.string s) <* Parsec.notFollowedBy (Parsec.oneOf valid_op_chars)
        char c = Parsec.char c <* Parsec.notFollowedBy (Parsec.oneOf valid_op_chars)

        parse_oper_symbol = (
            char '+' *> return Plus   <|>
            char '-' *> return Minus  <|>
            char '*' *> return Splat  <|>
            char '/' *> return Divide <|>
            str "//" *> return FloorDiv <|>
            char '%' *> return Modulo <|>
            char '^' *> return Hihat  <|>
            char '&' *> return FlipApply  <|>
            str "==" *> return Equals <|>
            str "=/=" *> return NotEquals <|>
            str "=~" *> return RegexMatch <|>
            str "<>" *> return Combine <|>
            char '>' *> return GreaterThan <|>
            char '<' *> return LesserThan <|>
            str "AND" *> return And <|> -- FIXME
            str "OR" *> return Or <|> -- FIXME
            str "><" *> return Xor <|>
            str "IN" *> return In <|> -- FIXME
            char ',' *> return Tuple <|>
            char '?' *> return Iff <|>
            str "??" *> return FromMaybe <|>
            str ">>" *> return Prepend <|>
            str "<<" *> return Append <|>
            char '#' *> return Index <|>
            str "##" *> return Lookup <|>
            str "~&" *> return Apply <|>
            char '&' *> return FlipApply <|>
            str "<~&>" *> return Map <|>
            str "<&>" *> return FlipMap <|>
            str "<~*>" *> return Applicative <|>
            str "<*>" *> return FlipApplicative <|>
            str "*>" *> return SequenceRight <|>
            str "<*" *> return SequenceLeft <|>
            str ">>=" *> return BindRight <|>
            str "=<<" *> return BindLeft
            ) <?> "infix operator"


parse_right_paren :: ShuntingYardParser OperToken
parse_right_paren = do
    spacing <- Parsec.optionMaybe respect_spaces
    _ <- Parsec.char ')'
    return $ case spacing of
        Nothing -> RParen
        Just () -> RParenAfterSpace
