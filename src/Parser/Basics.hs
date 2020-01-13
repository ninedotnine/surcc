module Parser.Basics where

import Debug.Trace

import Control.Monad (when)
import Text.Parsec hiding (space, spaces, string, newline)
import qualified Text.Parsec (string)
import qualified Data.Map.Strict  as Map (singleton, member)
import Data.List.NonEmpty ( NonEmpty(..) )

import Common
import Parser.ExprParser

data Endable_Stmts = Stmt_If_End | Stmt_While_End | Stmt_Unless_End | Stmt_Until_End

reserved :: String -> SouCParser String
reserved s = string s <* notFollowedBy identifier_char

reserved_word :: SouCParser String
reserved_word =
    choice (map reserved long_list) <?> "reserved word" where
        long_list = [
            -- these are in use, or i expect will be soon
            "if", "unless", "else", "while", "until",
            "for", "in", "do", "end", "where", "return",
            "break", "continue", "case", "and", "or",
            "atomic", "module", "import", "unary", "infix", "postfix",
            "typedef", "newtype", "datatype", "deriving", "typeclass",
            "define", "attribute", "assert", "trace", "undefined",

            -- these are reserved for future compatibility
            "abort", "abstract", "alias", "alignof", "allocate", "as",
            "associate", "asynchronous", "begin", "bind", "block",
            "breakpoint", "call", "close", "common", "const",
            "contains", "contiguous", "critical", "cycle", "data",
            "deallocate", "default", "defer", "deferred", "delegate",
            "dynamic", "elem", "element", "elif", "entry", "enum",
            "errno", "error", "eval", "exhibiting", "exhibits",
            "exists", "exit", "export", "explicit", "extend", "extends",
            "extern", "external", "fail", "final", "flush", "forall",
            "foreach", "format", "from", "function", "generic", "given",
            "global", "goto", "halt", "has", "implement", "implements",
            "implicit", "inquire", "instance", "intent", "interface",
            "internal", "is", "it", "kindof", "l", "label", "lambda",
            "let", "lock", "loop", "macro", "make", "match", "mem",
            "memory", "method", "mod", "namespace", "native", "new",
            "noop", "not", "null", "object", "only", "open", "operator",
            "override", "package", "parameter", "partial", "pass", "pause",
            "persist", "persistent", "pointer", "private", "procedure",
            "program", "public", "read", "recurse", "recursive", "ref",
            "require", "result", "rewind", "routine", "satisfies", "save",
            "select", "sequence", "sizeof", "static", "static_assert", "stop",
            "store", "struct", "sub", "subclass", "submodule", "subroutine",
            "suchthat", "super", "superclass", "switch", "sync", "synchronous",
            "table", "take", "target", "test", "then", "this", "to", "typeof",
            "unlock", "undef", "use", "virtual", "void", "volatile", "wait",
            "when", "with", "write", "yield", "zen"]

space :: SouCParser ()
space = char ' ' *> return ()

spaces :: SouCParser ()
spaces = many1 space *> return ()

newline :: SouCParser ()
newline = char '\n' *> return ()

keep_spaces :: SouCParser String
keep_spaces = many (char ' ')

endline :: SouCParser ()
endline = skipMany space *> (line_comment <|> block_comment <|> newline) <?> "end-of-line"

line_comment :: SouCParser ()
line_comment = char ';' *> manyTill anyChar newline *> return ()

block_comment :: SouCParser ()
block_comment = try (string "{;" *> notFollowedBy (char '>')) *> block_comment_depth 1 *> endline
    where
        nest n = string "{;" *> block_comment_depth n
        end = string ";}" *> return ()
        block_comment_depth :: Integer -> SouCParser ()
        block_comment_depth 1 = skipManyTill anyChar ((nest 2) <|> end)
        block_comment_depth n = skipManyTill anyChar ((nest (n+1)) <|> end *> block_comment_depth (n-1))

doc_comment :: SouCParser ()
doc_comment = string "{;>" *> skipManyTill anyChar (string "<;}") *> endline *> optional endline

-- pragma :: SouCParser Pragma -- FIXME
pragma :: SouCParser ()
pragma = string "{^;" *> space *> endBy1 (many1 alphaNum) space *> (string ";^}") *> endline <?> "pragma"

skipManyTill :: SouCParser a -> SouCParser b -> SouCParser ()
skipManyTill p1 p2 = manyTill p1 p2 *> return ()

-- Text.Parsec.string does this silly thing where it might fail while advancing the stream.
string :: String -> SouCParser String
string = try . Text.Parsec.string

double_newline :: SouCParser ()
double_newline = lookAhead (newline *> newline) *> newline -- something like this?

identifier_char :: SouCParser Char
identifier_char = (alphaNum <|> char '_')

upper_name :: SouCParser String
upper_name = do
    first <- upper
    rest <- many identifier_char
    return $ first:rest

identifier :: SouCParser Identifier
identifier = Identifier <$> raw_identifier

raw_identifier :: SouCParser String
raw_identifier = do
    notFollowedBy reserved_word
    first <- lower <|> char '_'
    rest <- many identifier_char
    return (first:rest)

-- for pattern matching
pattern :: SouCParser Param
pattern = Param <$> identifier `sepBy1` oneOf ","


increase_indent_level :: SouCParser ()
increase_indent_level = modifyState (\(x,m) -> (x+1,m))

decrease_indent_level :: SouCParser ()
decrease_indent_level = modifyState (\(x,m) -> (x-1,m))

indent_depth :: SouCParser ()
indent_depth = do
    (level, _) <- getState
    count level tab *> return () <?> "indent"

add_to_bindings :: Identifier -> ASTree -> SouCParser ()
add_to_bindings key val = do
    (i, (binds :| deeper_binds)) <- getState
    when (Map.member key binds)
        (parserFail ("constant `" ++ show key ++ "` already defined"))
    putState (i, ((binds <> Map.singleton key val) :| deeper_binds))

parens :: SouCParser a -> SouCParser a
parens = between (char '(') (char ')')

optional_do :: SouCParser ()
optional_do = skipMany space *> optional (reserved "do") *> return ()

optional_end_name :: Identifier -> SouCParser ()
optional_end_name (Identifier name) = do
    optional (try (endline *> indent_depth *> reserved "end"))
    optional (try (spaces *> string name))

optional_end :: Endable_Stmts -> SouCParser String
optional_end stmt_type = do
    keyword <- optionMaybe (try (endline *> indent_depth *> reserved "end"))
    name <- optionMaybe (try (spaces *> string word))
    -- FIXME also allow type annotation here
    lookAhead endline
    return (show keyword ++ " " ++ word ++ " " ++ show name)
        where word = case stmt_type of
                Stmt_While_End -> "while"
                Stmt_If_End -> "if"
                Stmt_Until_End -> "until"
                Stmt_Unless_End -> "unless"
