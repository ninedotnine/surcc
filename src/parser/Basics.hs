module Basics where

import Debug.Trace

import Control.Monad (when)
import Text.Parsec hiding (space, spaces, string, newline)
import qualified Text.Parsec
import qualified Data.Map.Strict  as Map (singleton, member)
import Data.List.NonEmpty ( NonEmpty(..) )

import SouC_Types

reserved_words :: Parser String
reserved_words = (foldr (<|>) (string "if") $ map (\s -> string s <* notFollowedBy identifier_char) [ "if", "unless", "else", "while", "until", "for", "in", "do", "end", "where", "return", "break", "continue", "case", "and", "or", "atomic", "module", "import", "unary", "infix", "postfix", "typedef", "newtype", "datatype", "deriving", "typeclass", "define", "attribute", "assert", "trace", "undefined",
    "abort", "abstract", "alias", "alignof", "allocate", "as", "associate", "asynchronous", "begin", "bind", "block", "breakpoint", "call", "close", "common", "const", "contains", "contiguous", "critical", "cycle", "data", "deallocate", "default", "defer", "deferred", "delegate", "dynamic", "elem", "element", "elif", "entry", "enum", "errno", "error", "eval", "exhibiting", "exhibits", "exists", "exit", "export", "explicit", "extend", "extends", "extern", "external", "fail", "final", "flush", "forall", "foreach", "format", "from", "function", "generic", "given", "global", "goto", "halt", "has", "implement", "implements", "implicit", "inquire", "instance", "intent", "interface", "internal", "is", "it", "kindof", "l", "label", "lambda", "let", "lock", "loop", "macro", "make", "match", "mem", "memory", "method", "mod", "namespace", "native", "new", "noop", "not", "null", "object", "only", "open", "operator", "override", "package", "parameter", "partial", "pass", "pause", "persist", "persistent", "pointer", "private", "procedure", "program", "public", "read", "recurse", "recursive", "ref", "require", "result", "rewind", "routine", "satisfies", "save", "select", "sequence", "sizeof", "static", "static_assert", "stop", "store", "struct", "sub", "subclass", "submodule", "subroutine", "suchthat", "super", "superclass", "switch", "sync", "synchronous", "table", "take", "target", "test", "then", "this", "to", "typeof", "unlock", "undef", "use", "virtual", "void", "volatile", "wait", "when", "with", "write", "yield"]) <?> "reserved word"

space :: Parser ()
space = char ' ' *> return ()

spaces :: Parser ()
spaces = many1 space *> return ()

newline :: Parser ()
newline = char '\n' *> return ()

keep_spaces :: Parser String
keep_spaces = many (char ' ')

endline :: Parser ()
endline = skipMany space *> (line_comment <|> block_comment <|> newline) <?> "end-of-line"

line_comment :: Parser ()
line_comment = char ';' *> manyTill anyChar newline *> return ()

block_comment :: Parser ()
block_comment = try (string "{;" *> notFollowedBy (char '>')) *> block_comment_depth 1 *> endline
    where
        nest n = string "{;" *> block_comment_depth n
        end = string ";}" *> return ()
        block_comment_depth :: Integer -> Parser ()
        block_comment_depth 1 = skipManyTill anyChar ((nest 2) <|> end)
        block_comment_depth n = skipManyTill anyChar ((nest (n+1)) <|> end *> block_comment_depth (n-1))

doc_comment :: Parser ()
doc_comment = string "{;>" *> skipManyTill anyChar (string "<;}") *> endline *> optional endline

-- pragma :: Parser Pragma -- FIXME
pragma :: Parser ()
pragma = string "{^;" *> space *> endBy1 (many1 alphaNum) space *> (string ";^}") *> endline <?> "pragma"

skipManyTill :: Parser a -> Parser b -> Parser ()
skipManyTill p1 p2 = manyTill p1 p2 *> return ()

-- Text.Parsec.string does this silly thing where it might fail while advancing the stream.
string :: String -> Parser String
string = try . Text.Parsec.string

double_newline :: Parser ()
double_newline = lookAhead (newline *> newline) *> newline -- something like this?

identifier_char :: Parser Char
identifier_char = (alphaNum <|> char '_')

upper_name :: Parser String
upper_name = do
    first <- upper
    rest <- many identifier_char
    return $ first:rest

identifier :: Parser Identifier
identifier = Identifier <$> raw_identifier

raw_identifier :: Parser String
raw_identifier = do
    notFollowedBy (reserved_words)
    first <- lower <|> char '_'
    rest <- many identifier_char
    return (first:rest)

-- for pattern matching
pattern :: Parser [Identifier]
pattern = identifier `sepBy1` oneOf ","


increase_indent_level :: Parser ()
increase_indent_level = modifyState (\(x,m) -> (x+1,m))

decrease_indent_level :: Parser ()
decrease_indent_level = modifyState (\(x,m) -> (x-1,m))

indent_depth :: Parser ()
indent_depth = do
    (level, _) <- getState
    count (4 * level) space *> return () <?> "indent" -- FIXME indent shouldn't have to be exactly 4 spaces

add_to_bindings :: Identifier -> Raw_Expr -> Parser ()
add_to_bindings key val = do
    (i, (binds :| deeper_binds)) <- getState
    when (Map.member key binds)
        (parserFail ("constant `" ++ value key ++ "` already defined"))
    putState (i, ((binds <> Map.singleton key val) :| deeper_binds))

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

reserved :: String -> Parser String
reserved = string -- FIXME

optional_do :: Parser ()
optional_do = skipMany space *> optional (reserved "do") *> return ()

optional_end_name :: Identifier -> Parser ()
-- optional_end_name (Identifier name) = (try (endline *> reserved "end")) *> optional (spaces *> string name) *> return ()
optional_end_name (Identifier name) = do
    optional (try (endline *> indent_depth *> reserved "end"))
    optional (try (spaces *> string name))

{-
optional_end :: Endable_Stmts -> Parser ()
optional_end stmt_type = optional ((indent_depth *> reserved "end") *> optional (spaces *> string word)) *> return ()
    where word = case stmt_type of
            Stmt_While_End -> "while"
            Stmt_If_End -> "if"
            -}

optional_end :: Endable_Stmts -> Parser String
optional_end stmt_type = do
    keyword <- optionMaybe (try (endline *> indent_depth *> reserved "end"))
    name <- optionMaybe (try (spaces *> string word))
    return (show keyword ++ show name)
        where word = case stmt_type of
                Stmt_While_End -> "while"
                Stmt_If_End -> "if"
