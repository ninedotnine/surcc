module SurCC.Common.Parsing (
    identifier_char,
    line_comment,
    space,
    spaces,
    ignore_spaces,
    enclosed_spaces,
    space_or_tab,
    newline,
    op_chars,
    pragma,
    skipManyTill,
    reserved,
    reserved',
    raw_identifier,
    string,
    endline,
    type_signature,
    souc_type,
    souc_type_simple,
    souc_type_parameterized,
    optional_sig,
    literal,
) where

import Control.Monad (when)
import Data.Functor ((<&>), void)
import Data.Function
import Data.List (genericLength)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec hiding (string, space, spaces, newline)
import Text.Parsec qualified (string)

import SurCC.Common (Term(..), Literal(..))
import SurCC.Common.SoucTypes

identifier_char :: Parsec Text s Char
identifier_char = (alphaNum <|> char '_') <?> "identifier char"


line_comment :: Parsec Text s ()
line_comment = void
    (try (skipMany space_or_tab *> char ';') *> manyTill anyChar newline <?> "")

space :: Parsec Text s ()
space = void (char ' ' <?> "")

spaces :: Parsec Text s ()
spaces = void $ many1 space

ignore_spaces :: Parsec Text s ()
ignore_spaces = skipMany space

space_or_tab :: Parsec Text s ()
space_or_tab = space <|> void tab

enclosed_spaces :: Parsec Text s ()
enclosed_spaces = void (try (ignore_spaces *> newline *> tab)) <|> spaces

newline :: Parsec Text s ()
newline = void (char '\n' <?> "newline")


op_chars :: String
op_chars = "+-*/~!@#$%^&<>=,"


pragma :: Parsec Text s ()
pragma = string "{^;" *> space *> endBy1 (many1 alphaNum) space *> (string ";^}") *> endline <?> "pragma"

skipManyTill :: Parsec Text s a -> Parsec Text s b -> Parsec Text s ()
skipManyTill p1 p2 = void $ manyTill p1 p2


reserved :: String -> Parsec Text s ()
reserved = reserved' <&> try

-- for those occasional cases when the try is higher-up.
reserved' :: String -> Parsec Text s ()
reserved' s = void $ Text.Parsec.string s <* notFollowedBy identifier_char


raw_identifier :: Parsec Text s Text
raw_identifier = do
    first <- lower <|> char '_'
    rest <- many identifier_char
    let i = Text.pack (first:rest)
    when (Set.member i reserved_words) $
        unexpected ("keyword: " <> (first:rest))
    pure i

reserved_words :: Set.Set Text
reserved_words = Set.fromList [
            -- these are in use, or i expect will be soon
            "if", "unless", "else", "while", "until",
            "for", "in", "do", "end", "where", "return",
            "break", "continue", "case", "and", "or",
            "let", "var",
            "atomic", "module", "import", "unary", "infix", "postfix",
            "typedef", "newtype", "datatype", "deriving", "typeclass",
            "def", "define", "attribute", "assert", "trace", "undefined",

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
            "lock", "loop", "macro", "make", "match", "mem",
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


-- Text.Parsec.string does this silly thing where it might fail while advancing
-- the stream.
string :: String -> Parsec Text s Text
string s = Text.pack <$> try (Text.Parsec.string s)

endline :: Parsec Text s ()
endline = skipMany space *> (line_comment <|> newline) <?> "end-of-line"

optional_sig :: Parsec Text s (Maybe SoucType)
optional_sig = optionMaybe type_signature

type_signature :: Parsec Text s SoucType
type_signature = char ':' *> ignore_spaces *> souc_type

souc_type :: Parsec Text s SoucType
souc_type = do
    name <- upper_name
    args <- many $ between (char '(') (char ')') souc_type
    pure (SoucTypeCon name args)

souc_type_parameterized :: Parsec Text s SoucType
souc_type_parameterized = do
    name <- upper_name
    args <- many $ between (char '(') (char ')') souc_type_var
    pure (SoucTypeCon name args)

souc_type_var :: Parsec Text s SoucType
souc_type_var = SoucTypeVar <$> flip TypeVar (SoucKind 0) <$>
    ((try (char 'T' *> many1 digit) <&> read <&> Left)
    <|> (upper <&> Right))

souc_type_simple :: Parsec Text s SoucType
souc_type_simple = do
    n <- upper_name
    return $ SoucType n

upper_name :: Parsec Text s Text
upper_name = do
    first <- upper
    rest <- many alphaNum
    pure $ Text.pack $ first:rest


literal :: Parsec Text s Literal
literal = parse_num
      <|> parse_char
      <|> parse_string

parse_num :: Parsec Text s Literal
parse_num = LitInt <$> read <$> many1 digit

parse_char :: Parsec Text s Literal
parse_char = LitChar <$> ((char '\'') *> anyChar <* (char '\''))

parse_string :: Parsec Text s Literal
parse_string = LitString . Text.pack <$> ((char '\"') *> many (noneOf "\"") <* (char '\"'))
