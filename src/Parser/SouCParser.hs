{-# LANGUAGE LambdaCase #-}

module Parser.SouCParser where

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Map.Strict as Map (Map, fromList)
import Text.Parsec hiding (space, spaces, string)


import Common
import Parser.Basics
import Parser.SouC_Expr  (Raw_Expr(..), raw_expr)
import Parser.SouC_Stmts (stmt_block)
import Parser.ExprParser (parse_expression)
import Parser.TabChecker (check_tabs)

runSouCParser :: SourceName -> SoucModule -> [ImportDecl] -> String -> Either ParseError Program
runSouCParser source_name (SoucModule name exports) imps input = do
    check_tabs source_name input
    body <- runParser souCParser (start_state name imps) source_name input
    pure $ Program (SoucModule name exports) imps body


type ModuleName = String
start_state :: ModuleName -> [ImportDecl] -> ParserState
start_state name imps = (0, binds:|[])
    where
        make_identifier :: ImportDecl -> Identifier
        make_identifier = \case
            LibImport n -> Identifier n
            RelImport n -> Identifier n
        binds = Map.fromList (zip ids (repeat Immut))
        ids = (Identifier name) : map make_identifier imps


souCParser :: SouCParser [Top_Level_Defn]
souCParser = do
    _ <- many (pragma) *> skipMany endline -- FIXME do something with pragmas
    code <- parseDefs
    eof
    pure $ code

parseDefs :: SouCParser [Top_Level_Defn]
parseDefs = do
    defns <- many (parse_def <* many1 endline)
    pure defns

parse_def :: SouCParser Top_Level_Defn
parse_def = do
    optional doc_comment
    defn <- main_defn
            <|> top_level_const
            <|> top_level_proc
            <|> (pragma *> skipMany endline *> parse_def)
            <?> "top-level definition"
    pure defn

main_defn :: SouCParser Top_Level_Defn
main_defn = do
    _ <- string "main("
    param <- optionMaybe pattern <* char ')'
    sig <- optionMaybe type_signature
    _ <- spaces <* char '=' <* spaces <* string "do" <* endline
    stmts <- stmt_block
    optional_end_name (Identifier "main")
    add_to_bindings (Identifier "main") Immut
    pure $ MainDefn param sig stmts

top_level_const :: SouCParser Top_Level_Defn
top_level_const = do
    name <- try (identifier <* lookAhead (char ':' <|> char ' '))
    m_sig <- optional_sig
    _ <- spaces <* char '='
    Raw_Expr val <- spaces *> raw_expr
    case parse_expression val of
        Right expr -> do
            add_to_bindings name Immut
            pure $ Top_Level_Const_Defn name m_sig expr
        Left err -> parserFail $ "invalid expression:\n" ++ show err

top_level_proc :: SouCParser Top_Level_Defn
top_level_proc = do
    proc_name <- try (identifier <* char '(')
    name <- (try (top_level_func proc_name) <|> top_level_sub proc_name)
    pure name

top_level_func :: Identifier -> SouCParser Top_Level_Defn
top_level_func func_name = do
    param <- pattern <* char ')' <* notFollowedBy (char ':' *> spaces *> string "IO")
    sig <- optionMaybe type_signature
    _ <- spaces <* char '=' <* spaces
    long_top_level_func func_name param sig <|> short_top_level_func func_name param sig

short_top_level_func :: Identifier -> Param -> Maybe SoucType -> SouCParser Top_Level_Defn
short_top_level_func func_name param sig = do
    (Raw_Expr body) <- raw_expr
    case parse_expression body of
        Right result -> do
            add_to_bindings func_name Immut
            pure $ ShortFuncDefn func_name param sig result
--         Left parse_err -> mergeError (fail "invalid expression") parse_err
        Left err -> fail $ "invalid expression\n" ++ show err

long_top_level_func :: Identifier -> Param -> Maybe SoucType -> SouCParser Top_Level_Defn
long_top_level_func func_name param sig = do
    stmts <- string "do" *> endline *> stmt_block
    optional_end_name func_name
    add_to_bindings func_name Immut
    pure $ FuncDefn func_name param sig stmts

top_level_sub :: Identifier -> SouCParser Top_Level_Defn
top_level_sub sub_name = do
    param <- optionMaybe pattern <* char ')'
    sig <- optionMaybe type_signature
    _ <- spaces <* char '=' <* spaces <* string "do" <* endline
    stmts <- stmt_block
    optional_end_name sub_name
    add_to_bindings sub_name Immut
    pure $ SubDefn sub_name param sig stmts
