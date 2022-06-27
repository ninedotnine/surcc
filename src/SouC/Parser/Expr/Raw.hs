module SouC.Parser.Expr.Raw (
    RawExpr(..),
    raw_expr,
    run_raw_expr_parser,
    postfix_oper -- fixme why is this in this module?
) where


import Debug.Trace

import Text.Parsec hiding (newline, space, spaces, string)
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map.Strict qualified as Map (Map, empty)
import Data.Text (Text)
import Data.Text qualified as Text

import SouC.Parser.Common
import SouC.Common.Parsing
import SouC.Parser.Expr.Types (RawExpr(..))

run_raw_expr_parser :: Text -> Text
run_raw_expr_parser input = let empty_state = (0, Map.empty :| []) in
    case runParser (raw_expr <* eof) empty_state "raw_expr" input of
        Left err -> "error: " <> Text.pack (show err)
        Right r -> Text.pack (show r)

raw_expr :: SouCParser RawExpr
raw_expr = RawExpr <$> Text.pack <$> dumb_raw_expr

 -- FIXME eventually this must skip newlines when inside parens, brackets, braces
dumb_raw_expr :: SouCParser String
dumb_raw_expr = fmap pure expr_char <> manyTill expr_char (lookAhead (eol <|> do_keyword)) where
    expr_char = oper_char <|> funky_expr_char <|> identifier_char
    do_keyword = try (string "do" *> (oneOf " \n")) *> pure ()
    eol = try endline

funky_expr_char :: SouCParser Char
funky_expr_char = oneOf " \"'()[]{}:"

oper_char :: SouCParser Char
oper_char = infix_oper_char <|> prefix_oper_char where
    infix_oper_char  = oneOf "#$%&*+-/<=>?\\^|~,"
    prefix_oper_char = oneOf "@!"

postfix_oper :: SouCParser String
postfix_oper = many1 oper_char
