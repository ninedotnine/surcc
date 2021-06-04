module Parser.TabChecker (check_tabs) where

import Data.Text (Text)
import Text.Parsec hiding (space, spaces, string, newline)
import Text.Parsec.Error

import Common.Parsing

type TabCheckingParser a = Parsec Text () a

msg :: Message
msg = Message "it looks like you tried to indent with spaces instead of tabs?"

check_tabs :: FilePath -> Text -> Either ParseError ()
check_tabs name input = case runParser tab_checker () name input of
        Right () -> Right ()
        Left err -> Left $ setErrorMessage msg err

tab_checker :: TabCheckingParser ()
tab_checker = many (valid_line <|> blank_line <|> comment_line) *> (eof <?> "")

valid_line :: TabCheckingParser ()
valid_line = try (noneOf " " *> manyTill anyChar newline) *> pure ()

blank_line :: TabCheckingParser ()
blank_line = try (many space_or_tab *> newline) *> pure ()

comment_line :: TabCheckingParser ()
comment_line = (skipMany space_or_tab *> comment_leader) *> manyTill anyChar newline *> pure ()

comment_leader :: TabCheckingParser ()
comment_leader = char ';' *> pure ()
