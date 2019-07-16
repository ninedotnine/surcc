module Basics where

import Debug.Trace

import Control.Monad (when)
import Text.Parsec hiding (space, spaces, string)
import qualified Text.Parsec

import SouC_Types

reserved_words :: Parser String
reserved_words = (foldr (<|>) (try (string "if")) $ map (try . string) [ "if", "unless", "else", "while", "until", "for", "in", "do", "end", "where", "return", "break", "continue", "case", "and", "or", "atomic", "module", "import", "unary", "infix", "postfix", "typedef", "newtype", "datatype", "deriving", "typeclass", "define", "attribute", "assert", "trace", "undefined", "abort", "abstract", "alias", "alignof", "allocate", "as", "associate", "asynchronous", "begin", "bind", "block", "breakpoint", "call", "close", "common", "const", "contains", "contiguous", "critical", "cycle", "data", "deallocate", "default", "defer", "deferred", "delegate", "dynamic", "elem", "element", "elif", "entry", "enum", "errno", "error", "eval", "exhibiting", "exhibits", "exists", "exit", "export", "explicit", "extend", "extends", "extern", "external", "fail", "final", "flush", "forall", "foreach", "format", "from", "function", "generic", "given", "global", "goto", "halt", "has", "implement", "implements", "implicit", "inquire", "instance", "intent", "interface", "internal", "is", "it", "kindof", "l", "label", "lambda", "let", "lock", "loop", "macro", "make", "match", "mem", "memory", "method", "mod", "namespace", "native", "new", "noop", "not", "null", "object", "only", "open", "operator", "override", "package", "parameter", "partial", "pass", "pause", "persist", "persistent", "pointer", "private", "procedure", "program", "public", "read", "recurse", "recursive", "ref", "require", "result", "rewind", "routine", "satisfies", "save", "select", "sequence", "sizeof", "static", "static_assert", "stop", "store", "struct", "sub", "subclass", "submodule", "subroutine", "suchthat", "super", "superclass", "switch", "sync", "synchronous", "table", "take", "target", "test", "then", "this", "to", "typeof", "unlock", "undef", "use", "virtual", "void", "volatile", "wait", "when", "with", "write", "yield"]) <?> "reserved word"

space :: Parser ()
space = char ' ' *> return ()

spaces :: Parser ()
spaces = many1 space *> return ()

endline :: Parser ()
-- endline = try (skipMany space *> newline) *> return () <?> "end-of-line"
endline = try (skipMany space *> newline) *> return () <?> "end-of-line"


string :: String -> Parser String
string = try . Text.Parsec.string

double_newline :: Parser ()
double_newline = lookAhead (newline *> newline) *> newline *> return () -- something like this?


upper_name :: Parser String
upper_name = do
    first <- upper
    rest <- many (alphaNum <|> char '_')
    return $ first:rest

identifier :: Parser Identifier
identifier = do
--     notFollowedBy (fmap string reserved_words)
--     notFollowedBy (reserved_words)
    first <- lower <|> char '_'
    rest <- many (alphaNum <|> char '_')
--     when $ (first:rest) `elem` reserved_words
--         fail
    return $ Identifier(first:rest)

-- for pattern matching
pattern :: Parser [Identifier]
pattern = identifier `sepBy1` oneOf ","


increase_indent_level :: Parser ()
increase_indent_level = modifyState (+1)

decrease_indent_level :: Parser ()
decrease_indent_level = modifyState (\x -> x-1)

indent_depth :: Parser ()
indent_depth = do
    level <- getState
    count (4 * level) space *> return () -- FIXME indent shouldn't have to be exactly 4 spaces

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
