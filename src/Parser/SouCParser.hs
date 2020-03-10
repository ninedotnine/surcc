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
    name <- module_name <|> return Nothing
    _ <- many (pragma) *> skipMany endline -- FIXME do something with pragmas
    imps <- imports
    code <- parseDefs
    eof
    return $ Program name imps code -- FIXME return something useful

module_name :: SouCParser (Maybe ModuleName)
module_name = do
    name <- string "module" *> space *> raw_identifier <* endline <* (many pragma) <* endline
    return $ Just (ModuleName(name))

imports :: SouCParser Imports
imports = do
    imps <- many souc_import
    -- FIXME a blank line is required before any code
    return imps

souc_import :: SouCParser Import
souc_import = do
    name <- string "import" *> spaces *> module_path <* skipMany1 endline
    return $ Import(name)

parseDefs :: SouCParser [Top_Level_Defn]
parseDefs = do
    defns <- many (parse_def <* many1 endline)
    return defns

parse_def :: SouCParser Top_Level_Defn
parse_def = do
    optional doc_comment
    defn <- main_defn
            <|> top_level_const
            <|> top_level_proc
            <|> (pragma *> skipMany endline *> parse_def)
            <?> "top-level definition"
    return defn

main_defn :: SouCParser Top_Level_Defn
main_defn = do
    _ <- string "main("
    param <- optionMaybe pattern <* char ')' <* spaces <* char '=' <* spaces <* string "do" <* endline
    stmts <- stmt_block
    optional_end_name (Identifier "main")
    return $ MainDefn param stmts

top_level_const :: SouCParser Top_Level_Defn
top_level_const = do
    name <- try (identifier <* spaces <* char '=')
    Raw_Expr val <- spaces *> raw_expr
    case parse_expression val of
        Right expr -> do
            add_to_bindings name expr
            return $ Top_Level_Const_Defn name expr
        Left err -> parserFail $ "invalid expression:\n" ++ show err

top_level_proc :: SouCParser Top_Level_Defn
top_level_proc = do
    proc_name <- try (identifier <* char '(')
    return =<< (try (top_level_func proc_name) <|> top_level_sub proc_name)

top_level_func :: Identifier -> SouCParser Top_Level_Defn
top_level_func func_name = do
    param <- pattern <* char ')' <* spaces <* char '=' <* spaces
    short_top_level_func func_name param <|> long_top_level_func func_name param

short_top_level_func :: Identifier -> Param -> SouCParser Top_Level_Defn
short_top_level_func func_name param = do
    (Raw_Expr body) <- raw_expr
    case parse_expression body of
        Right result -> return $ ShortFuncDefn func_name param result
--         Left parse_err -> mergeError (fail "invalid expression") parse_err
        Left err -> fail $ "invalid expression\n" ++ show err

long_top_level_func :: Identifier -> Param -> SouCParser Top_Level_Defn
long_top_level_func func_name param = do
    stmts <- string "do" *> endline *> stmt_block
    optional_end_name func_name
    return $ FuncDefn func_name param stmts

top_level_sub :: Identifier -> SouCParser Top_Level_Defn
top_level_sub sub_name = do
    param <- optionMaybe pattern <* char ')' <* spaces <* char '=' <* spaces <* string "do" <* endline
    stmts <- stmt_block
    optional_end_name sub_name
    return $ SubDefn sub_name param stmts
