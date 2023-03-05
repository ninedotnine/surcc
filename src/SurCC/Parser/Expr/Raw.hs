module SurCC.Parser.Expr.Raw (
    raw_expr,
--     run_raw_expr_parser,
    postfix_oper -- fixme why is this in this module?
) where


import Debug.Trace

import Text.Parsec hiding (newline, space, spaces, string)
import Text.Parsec qualified
import Data.Functor
import Data.List (intersperse)
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map.Strict qualified as Map (Map, empty)
import Data.Text (Text)
import Data.Text qualified as Text

import SurCC.Parser.Common
import SurCC.Common.Parsing
import SurCC.Parser.Expr.Types (RawExpr(..))

--
-- run_raw_expr_parser :: Text -> Text
-- run_raw_expr_parser input = let empty_state = (0, Map.empty :| []) in
--     case runParser (raw_expr) empty_state "raw_expr" input of
--         Left err -> "error: " <> Text.pack (show err)
--         Right r -> Text.pack (show r)
--

-- this function stupidly grabs characters
-- until a newline
-- without consuming that newline
raw_expr :: Parsec Text s RawExpr
raw_expr = RawExpr <$> Text.pack <$> dumb_raw_expr


 -- FIXME eventually this must skip newlines when inside parens, brackets, braces
dumb_raw_expr :: Parsec Text s String
dumb_raw_expr = fmap pure expr_char <> manyTill expr_char (lookAhead (eol <|> do_keyword)) where
    expr_char = oper_char <|> funky_expr_char <|> identifier_char
    do_keyword = void $ try (reserved' "do" *> (oneOf " \n"))
    eol = try (endline <|> eof)


funky_expr_char :: Parsec Text s Char
funky_expr_char = oneOf " \"'()[]{}:."

oper_char :: Parsec Text s Char
oper_char = oneOf "!@#$%&*+-/<=>?\\^|~,"

postfix_oper :: Parsec Text s String
postfix_oper = many1 oper_char
