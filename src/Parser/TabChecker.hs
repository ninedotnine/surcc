module Parser.TabChecker (check_tabs) where

import Text.Parsec hiding (char, space, spaces, string)
import qualified Text.Parsec as Parsec
import Text.Parsec.Error

type TabCheckingParser a = Parsec String () a

msg :: Message
msg = Message "it looks like you tried to indent with spaces instead of tabs?"

check_tabs :: FilePath -> String -> Either ParseError ()
check_tabs name input = case runParser tab_checker () name input of
        Right () -> Right ()
        Left err -> Left $ setErrorMessage msg err

tab_checker :: TabCheckingParser ()
tab_checker = many (valid_line <|> blank_line <|> comment_line) *> (eof <?> "")

valid_line :: TabCheckingParser ()
valid_line = try (noneOf " " *> manyTill anyChar newline) *> return ()

blank_line :: TabCheckingParser ()
blank_line = try (many space_or_tab *> newline) *> return ()

comment_line :: TabCheckingParser ()
comment_line = (skipMany space_or_tab *> comment_leader) *> manyTill anyChar newline *> return ()

space_or_tab :: TabCheckingParser ()
space_or_tab = (char ' ' <|> char '\t')

comment_leader :: TabCheckingParser ()
comment_leader = char ';'

char :: Char -> TabCheckingParser ()
char c = Parsec.char c *> return () <?> ""
