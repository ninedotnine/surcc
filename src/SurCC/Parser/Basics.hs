module SurCC.Parser.Basics (
    identifier,
    param,
    indentation,
    add_to_bindings,
    bindings_lookup,
    optional_do,
    end_block,
    end_block_named,
    Endable_Stmts(..),
) where

import Control.Monad (when)
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map.Strict qualified as Map (singleton, member, lookup)
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Text.Parsec hiding (space, spaces, string, newline)

import SurCC.Common
import SurCC.Common.Parsing
import SurCC.Parser.Common

data Endable_Stmts = Stmt_If_End | Stmt_While_End | Stmt_Unless_End | Stmt_Until_End

blank_line :: SurCParser ()
blank_line = try (skipMany space_or_tab *> newline)

identifier :: SurCParser Identifier
identifier = Identifier <$> raw_identifier <?> "identifier"

-- a param might have a type sig
param :: SurCParser Param
param = do
    name <- identifier
    sig <- optionMaybe type_signature
    pure (Param name sig)


indentation :: SurCParser ()
indentation = try $ do
    (level, _) <- getState
    count level tab *> pure () <?> "indentation"

add_to_bindings :: Identifier -> Mutability -> SurCParser ()
add_to_bindings key val = do
    (i, (binds :| deeper_binds)) <- getState
    found <- bindings_contains key
    when found
        (parserFail ("constant or variable `" ++ show key ++ "` already defined"))
    putState (i, ((binds <> Map.singleton key val) :| deeper_binds))

-- fixme
-- i think sometimes a variable cannot be reassigned, even if it is visible
-- from the current scope and is mutable.
-- nested function definitions, for example?
-- that's why i might need this function
add_to_current_scope :: Identifier -> Mutability -> SurCParser ()
add_to_current_scope key val = do
    (i, (binds :| deeper_binds)) <- getState
    when (Map.member key binds)
        (parserFail ("constant `" ++ show key ++ "` already defined"))
    putState (i, ((binds <> Map.singleton key val) :| deeper_binds))


bindings_contains :: Identifier -> SurCParser Bool
bindings_contains i = do
    (_ , list) <- getState
    pure $ foldr f False list
        where
            f binds found = Map.member i binds || found

bindings_lookup :: Identifier -> SurCParser (Maybe Mutability)
bindings_lookup i = do
    (_ , list) <- getState
    pure $ foldr f Nothing list
        where
            f binds found = if isJust found then found else Map.lookup i binds


optional_do :: SurCParser ()
optional_do = skipMany space *> optional (reserved "do") *> pure ()


end_block_named :: Identifier -> SurCParser ()
end_block_named (Identifier name) = do
    _ <- lookAhead (try (indentation *> reserved' "end")) *>
            indentation *> reserved' "end"
    optional (try (spaces *> string (Text.unpack name)))
    endline

end_block :: Endable_Stmts -> SurCParser ()
end_block stmt_type = do
    try (indentation *> reserved' "end")
    optional (try (spaces *> string word))
    -- FIXME also allow type annotation here
    endline
        where word = case stmt_type of
                Stmt_While_End -> "while" :: String
                Stmt_If_End -> "if"
                Stmt_Until_End -> "until"
                Stmt_Unless_End -> "unless"
