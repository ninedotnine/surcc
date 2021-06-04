module Parser.Basics where

import Control.Monad (when)
import Text.Parsec hiding (space, spaces, string, newline)
import Data.Maybe (isJust)
import qualified Data.Map.Strict  as Map (singleton, member, lookup)
import Data.List.NonEmpty ( NonEmpty(..) )

import Common
import Common.Parsing

data Endable_Stmts = Stmt_If_End | Stmt_While_End | Stmt_Unless_End | Stmt_Until_End

blank_line :: SouCParser ()
blank_line = try (skipMany space_or_tab *> newline)

meaningless_fluff :: SouCParser ()
meaningless_fluff = skipMany (line_comment <|> blank_line)

double_newline :: SouCParser ()
double_newline = lookAhead (newline *> newline) *> newline -- something like this?

identifier :: SouCParser Identifier
identifier = Identifier <$> raw_identifier

-- for pattern matching
pattern :: SouCParser Param
pattern = do
    name <- identifier
    sig <- optionMaybe type_signature
    pure (Param name sig)

increase_indent_level :: SouCParser ()
increase_indent_level = modifyState (\(x,m) -> (x+1,m))

decrease_indent_level :: SouCParser ()
decrease_indent_level = modifyState (\(x,m) -> (x-1,m))

indent_depth :: SouCParser ()
indent_depth = do
    optional meaningless_fluff
    (level, _) <- getState
    count level tab *> pure () <?> "indent"

add_to_bindings :: Identifier -> Mutability -> SouCParser ()
add_to_bindings key val = do
    (i, (binds :| deeper_binds)) <- getState
    when (Map.member key binds)
        (parserFail ("constant `" ++ show key ++ "` already defined"))
    putState (i, ((binds <> Map.singleton key val) :| deeper_binds))

bindings_contains :: Identifier -> SouCParser Bool
bindings_contains i = do
    (_ , list) <- getState
    pure $ foldr f False list
        where
            f binds found = Map.member i binds || found

bindings_lookup :: Identifier -> SouCParser (Maybe Mutability)
bindings_lookup i = do
    (_ , list) <- getState
    pure $ foldr f Nothing list
        where
            f binds found = if isJust found then found else Map.lookup i binds


optional_do :: SouCParser ()
optional_do = skipMany space *> optional (reserved "do") *> pure ()

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
    pure (show keyword ++ " " ++ word ++ " " ++ show name)
        where word = case stmt_type of
                Stmt_While_End -> "while"
                Stmt_If_End -> "if"
                Stmt_Until_End -> "until"
                Stmt_Unless_End -> "unless"
