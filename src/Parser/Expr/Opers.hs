-- oper parsers
module Parser.Expr.Opers (
    parse_oper_token
) where

import Text.Parsec qualified as Parsec
import Text.Parsec ((<|>), (<?>))

import Parser.Expr.Types
import Parser.Expr.RegardingSpaces
import Parser.Expr.StackManipulations

parse_oper_token :: ShuntingYardParser OperToken
parse_oper_token =
    (check_for_oper *> apply_tight_prefix_opers *> parse_infix_oper) <|> parse_right_paren
    <?> "infix operator"

check_for_oper :: ShuntingYardParser ()
check_for_oper = Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.oneOf valid_op_chars)) *> pure ()

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
        char :: Char -> ShuntingYardParser Char
        str s = Parsec.try (Parsec.string s) <* Parsec.notFollowedBy (Parsec.oneOf valid_op_chars)
        char c = Parsec.char c <* Parsec.notFollowedBy (Parsec.oneOf valid_op_chars)

        parse_oper_symbol = (
            char '+' *> pure Plus   <|>
            char '-' *> pure Minus  <|>
            char '*' *> pure Splat  <|>
            str "//" *> pure FloorDiv <|>
            char '/' *> pure FieldDiv <|>
            char '%' *> pure Modulo <|>
            char '^' *> pure Hihat  <|>
            char '&' *> pure FlipApply  <|>
            str "==" *> pure Equals <|>
            str "=/=" *> pure NotEquals <|>
            str "=~" *> pure RegexMatch <|>
            str "<>" *> pure Combine <|>
            char '>' *> pure GreaterThan <|>
            char '<' *> pure LesserThan <|>
            str "AND" *> pure And <|> -- FIXME
            str "OR" *> pure Or <|> -- FIXME
            str "><" *> pure Xor <|>
            str "IN" *> pure In <|> -- FIXME
            char ',' *> pure Comma <|>
            char '?' *> pure Iff <|>
            str "??" *> pure FromMaybe <|>
            str ">>" *> pure Prepend <|>
            str "<<" *> pure Append <|>
            char '#' *> pure Index <|>
            str "##" *> pure Lookup <|>
            str "~&" *> pure Apply <|>
            char '&' *> pure FlipApply <|>
            str "<~&>" *> pure Map <|>
            str "<&>" *> pure FlipMap <|>
            str "<~*>" *> pure Applicative <|>
            str "<*>" *> pure FlipApplicative <|>
            str "*>" *> pure SequenceRight <|>
            str "<*" *> pure SequenceLeft <|>
            str ">>=" *> pure BindRight <|>
            str "=<<" *> pure BindLeft
            ) <?> "infix operator"


parse_right_paren :: ShuntingYardParser OperToken
parse_right_paren = do
    spacing <- Parsec.optionMaybe respect_spaces
    _ <- Parsec.char ')'
    pure $ case spacing of
        Nothing -> RParen
        Just () -> RParenAfterSpace
