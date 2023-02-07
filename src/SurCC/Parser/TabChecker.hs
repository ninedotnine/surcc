module SurCC.Parser.TabChecker (check_tabs) where

import Control.Arrow (left)
import Data.Functor (void)
import Data.Text (Text)
import Text.Parsec hiding (space, spaces, string, newline)
import Text.Parsec.Error

import SurCC.Common.Parsing

type TabCheckingParser a = Parsec Text () a

msg :: Message
msg = Message "it looks like you tried to indent with spaces instead of tabs?"

check_tabs :: FilePath -> Text -> Either ParseError ()
check_tabs name input = left (setErrorMessage msg) $
    runParser tab_checker () name input

tab_checker :: TabCheckingParser ()
tab_checker = many (valid_line <|> blank_line <|> comment_line) *> (eof <?> "")

valid_line :: TabCheckingParser ()
valid_line = void $ try (noneOf " " *> manyTill anyChar newline)

blank_line :: TabCheckingParser ()
blank_line = void $ try (many space_or_tab *> newline)

comment_line :: TabCheckingParser ()
comment_line = void $
    skipMany space_or_tab *> comment_leader *> manyTill anyChar newline

comment_leader :: TabCheckingParser ()
comment_leader = void (char ';')
