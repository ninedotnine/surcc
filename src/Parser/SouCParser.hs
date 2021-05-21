module Parser.SouCParser where

-- import System
-- import System.FilePath
import Text.Parsec hiding (space, spaces, string)
-- import Text.Parsec.Expr
-- import Text.Parsec.String
-- iortsmport Control.Monad

-- import Souc_Expr
import Parser.Basics
import Common
import Parser.SouC_Expr
import Parser.SouC_Stmts
import Parser.ExprParser
import Parser.TabChecker

runSouCParser :: SourceName -> String -> Either ParseError Program
runSouCParser name input =
    check_tabs name input >> runParser souCParser empty_state name input

souCParser :: SouCParser Program
souCParser = do
    name <- optionMaybe module_header
    _ <- many (pragma) *> skipMany endline -- FIXME do something with pragmas
    imps <- imports
    code <- parseDefs
    eof
    pure $ Program name imps code -- FIXME pure something useful

module_header :: SouCParser SoucModule
module_header = do
    name <- string "module" *> space *> raw_identifier
    add_to_bindings (Identifier name) Immut
    decls <- optionMaybe export_decls
    case decls of
        Just exports ->
            pure $ SoucModule name exports
        Nothing -> do
            endline <* many pragma <* endline
            pure $ SoucModule name []

export_decls :: SouCParser [ExportDecl]
export_decls = do
    try (space *> reserved "where") *> endline
    many1 (tab *> export_decl)

export_decl :: SouCParser ExportDecl
export_decl = do
    i <- identifier <* optional spaces
    t <- type_signature <* endline
    pure (ExportDecl i t)


imports :: SouCParser Imports
imports = do
    imps <- many souc_import
    -- FIXME a blank line is required before any code
    pure imps

souc_import :: SouCParser Import
souc_import = do
    name <- string "import" *> spaces *> module_path <* skipMany1 endline
    add_to_bindings (Identifier name) Immut
    pure $ Import(name)

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
