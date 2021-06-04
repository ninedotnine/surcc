module Imports.Parser (
    parse_module_header
) where

import Text.Parsec hiding (string, space, spaces, newline)
import qualified Text.Parsec (string)

import Common
import Parser.SouC_Expr  (Raw_Expr(..), raw_expr)
import Parser.SouC_Stmts (stmt_block)
import Parser.ExprParser (parse_expression)
import Parser.TabChecker (check_tabs)

import Debug.Trace

type HeaderParser a = Parsec String [ImportDecl] a

add_to_imports :: ImportDecl -> HeaderParser ()
add_to_imports name = modifyState (\l -> name:l)

parse_module_header :: SourceName -> String -> Either ParseError (SoucModule, [ImportDecl], String)
parse_module_header name input = runParser module_header_and_imports [] name input

module_header_and_imports :: HeaderParser (SoucModule, [ImportDecl], String)
module_header_and_imports = do
    m_header <- optionMaybe module_header

    _ <- many (pragma) *> skipMany endline -- FIXME do something with pragmas
    imps <- import_list
    rest <- getInput
    pure (spoof_module m_header, imps, rest)

spoof_module :: Maybe SoucModule -> SoucModule
spoof_module = \case
    Just m -> m
    Nothing -> SoucModule "anonymous_main_module" []


module_header :: HeaderParser SoucModule
module_header = do
    name <- string "module" *> space *> raw_identifier
    decls <- optionMaybe export_decls
    case decls of
        Just exports ->
            pure $ SoucModule name exports
        Nothing -> do
            endline <* many pragma <* endline
            pure $ SoucModule name []

export_decls :: HeaderParser [ExportDecl]
export_decls = do
    try (space *> reserved "where") *> endline
    many1 (tab *> export_decl)

export_decl :: HeaderParser ExportDecl
export_decl = do
    i <- raw_identifier <* optional spaces
    t <- type_signature <* endline
    pure (ExportDecl (Identifier i) t)


import_list :: HeaderParser [ImportDecl]
import_list = many souc_import
    -- FIXME a blank line is required before any code


souc_import :: HeaderParser ImportDecl
souc_import = do
    name <- reserved "import" *> spaces *> module_path <* skipMany1 endline
    pure (LibImport name)



----- FIXME
-- basically everything after this is duplicated from Parser.Basics

raw_identifier :: HeaderParser String
raw_identifier = do
    notFollowedBy reserved_word
    first <- lower <|> char '_'
    rest <- many identifier_char
    pure (first:rest)

-- Text.Parsec.string does this silly thing where it might fail while advancing the stream.
string :: String -> HeaderParser String
string = try . Text.Parsec.string

endline :: HeaderParser ()
endline = skipMany space *> (line_comment <|> block_comment <|> newline) <?> "end-of-line"


type_signature :: HeaderParser SoucType
type_signature = char ':' *> spaces *> type_broadly where

    type_broadly :: HeaderParser SoucType
    type_broadly = try type_constructor <|> simple_type

    type_constructor :: HeaderParser SoucType
    type_constructor = do
        name <- type_name <* char '('
        args <- sepBy1 type_broadly spaces <* char ')'
        pure (SoucTypeConstructor name args)

    simple_type :: HeaderParser SoucType
    simple_type = SoucType <$> type_name

    type_name :: HeaderParser String
    type_name = do
        first <- upper
        rest <- many (lower <|> upper <|> digit)
        pure (first:rest)

module_path :: HeaderParser String
module_path = do
    leading_slash <- option "" slash
    dir <- many $ lookAhead (try (name <> slash)) *> (name <> slash)
    path <- raw_identifier
    pure (leading_slash ++ concat dir ++ path)
        where
            dot = string "."
            dotdot = string ".."
            slash = string "/"
            name = (many1 identifier_char) <|> dotdot <|> dot


identifier_char :: HeaderParser Char
identifier_char = (alphaNum <|> char '_')

line_comment :: HeaderParser ()
line_comment = try (skipMany space_or_tab *> char ';') *> manyTill anyChar newline *> pure () <?> ""

block_comment :: HeaderParser ()
block_comment = try (string "{;" *> notFollowedBy (char '>')) *> block_comment_depth 1 *> endline <?> ""
    where
        nest n = string "{;" *> block_comment_depth n
        end = string ";}" *> pure ()
        block_comment_depth :: Integer -> HeaderParser ()
        block_comment_depth 1 = skipManyTill anyChar ((nest 2) <|> end)
        block_comment_depth n = skipManyTill anyChar ((nest (n+1)) <|> end *> block_comment_depth (n-1))

doc_comment :: HeaderParser ()
doc_comment = string "{;>" *> skipManyTill anyChar (string "<;}") *> endline *> optional endline <?> ""


space :: HeaderParser ()
space = char ' ' *> pure () <?> ""

spaces :: HeaderParser ()
spaces = many1 space *> pure ()

space_or_tab :: HeaderParser ()
space_or_tab = space <|> tab *> pure ()

newline :: HeaderParser ()
newline = char '\n' *> pure () <?> "newline"


-- pragma :: SouCParser Pragma -- FIXME
pragma :: HeaderParser ()
pragma = string "{^;" *> space *> endBy1 (many1 alphaNum) space *> (string ";^}") *> endline <?> "pragma"

skipManyTill :: HeaderParser a -> HeaderParser b -> HeaderParser ()
skipManyTill p1 p2 = manyTill p1 p2 *> pure ()

reserved :: String -> HeaderParser String
reserved s = string s <* notFollowedBy identifier_char


reserved_word :: HeaderParser String
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
            "when", "with", "yield", "zen"]

