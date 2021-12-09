module Common.Parsing (
    identifier_char,
    line_comment,
    space,
    spaces,
    space_or_tab,
    newline,
    pragma,
    skipManyTill,
    reserved,
    reserved_word,
    raw_identifier,
    string,
    endline,
    type_signature,
    type_name,
    upper_name,
    optional_sig,
    constructor_name
) where

import Data.List (genericLength)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec hiding (string, space, spaces, newline)
import qualified Text.Parsec (string)

import Common (SoucKind(..), SoucType(..), Term(..))

identifier_char :: Parsec Text s Char
identifier_char = (alphaNum <|> char '_')


line_comment :: Parsec Text s ()
line_comment = try (skipMany space_or_tab *> char ';') *> manyTill anyChar newline *> pure () <?> ""

space :: Parsec Text s ()
space = char ' ' *> pure () <?> ""

spaces :: Parsec Text s ()
spaces = many1 space *> pure ()

space_or_tab :: Parsec Text s ()
space_or_tab = space <|> tab *> pure ()

ignore_spaces :: Parsec Text s ()
ignore_spaces = skipMany (char ' ' <?> "")

newline :: Parsec Text s ()
newline = (char '\n' *> pure ()) <?> "newline"


pragma :: Parsec Text s ()
pragma = string "{^;" *> space *> endBy1 (many1 alphaNum) space *> (string ";^}") *> endline <?> "pragma"

skipManyTill :: Parsec Text s a -> Parsec Text s b -> Parsec Text s ()
skipManyTill p1 p2 = manyTill p1 p2 *> pure ()

reserved_specific :: String -> Parsec Text s Text
reserved_specific s = string s <* notFollowedBy identifier_char

reserved :: String -> Parsec Text s ()
reserved s = reserved_specific s *> pure ()

reserved_word :: Parsec Text s Text
reserved_word =
    choice (map reserved_specific long_list) <?> "reserved word" where
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



raw_identifier :: Parsec Text s Text
raw_identifier = do
    notFollowedBy reserved_word
    first <- lower <|> char '_'
    rest <- many identifier_char
    pure (Text.pack (first:rest))

-- Text.Parsec.string does this silly thing where it might fail while advancing
-- the stream.
string :: String -> Parsec Text s Text
string s = Text.pack <$> try (Text.Parsec.string s)

endline :: Parsec Text s ()
endline = skipMany space *> (line_comment <|> newline) <?> "end-of-line"

optional_sig :: Parsec Text s (Maybe SoucType)
optional_sig = optionMaybe type_signature

type_signature :: Parsec Text s SoucType
type_signature = char ':' *> ignore_spaces *> type_broadly where

    type_broadly :: Parsec Text s SoucType
    type_broadly = try type_constructor <|> type_name

    type_constructor :: Parsec Text s SoucType
    type_constructor = do
        name <- upper_name <* char '('
        args <- sepBy1 type_broadly spaces <* char ')'
        pure (SoucTypeConstructor name (SoucKind $ genericLength args) args)

upper_name :: Parsec Text s Text
upper_name = do
    first <- upper
    rest <- many alphaNum
    pure $ Text.pack $ first:rest

type_name :: Parsec Text s SoucType
type_name = do
    n <- upper_name
    return $ SoucType n (SoucKind 0)

constructor_name :: Parsec Text s Term
constructor_name = Constructor <$> upper_name
