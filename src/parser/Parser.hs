module Parser where

import Debug.Trace (traceM)

-- import System
-- import System.FilePath
import Text.Parsec hiding (space, spaces, string)
-- import Text.Parsec.Expr
-- import Text.Parsec.String
-- iortsmport Control.Monad

-- import Souc_Expr
import Basics
import SouC_Types
import SouC_Expr
import SouC_Stmts


runSouCParser :: SourceName -> String -> Either ParseError Program
runSouCParser name input = runParser souCParser initState name input
--     where initState = (0, [])
    where initState = 0

souCParser :: Parser Program
souCParser = do
    name <- module_name <|> return Nothing
    _ <- many (pragma) *> skipMany endline -- FIXME do something with pragmas
    imps <- imports
    code <- parseDefs
    eof
    return $ Program name imps code -- FIXME return something useful

module_name :: Parser (Maybe ModuleName)
module_name = do
    name <- string "module" *> space *> upper_name <* endline <* (many pragma) <* endline
    return $ Just (ModuleName(name))

imports :: Parser Imports
imports = do
    imps <- many souc_import
    -- FIXME a blank line is required before any code
    return imps

souc_import :: Parser Import
souc_import = do
    name <- string "import" *> spaces *> upper_name <* skipMany1 endline
    return $ Import(name)

parseDefs :: Parser [Top_Level_Defn]
parseDefs = do
    defns <- many (parse_def <* many1 endline)
    return defns

parse_def :: Parser Top_Level_Defn
parse_def = do
    optional doc_comment
    defn <- top_level_const <|> top_level_proc <|> (pragma *> skipMany endline *> parse_def) <?> "top-level definition"
    return defn

top_level_const :: Parser Top_Level_Defn
top_level_const = do
    const_defn <- try stmt_const_assign
    case const_defn of
        Stmt_Const_Assign iden val -> return $ Top_Level_Const_Defn iden val
        _ -> return undefined

top_level_proc :: Parser Top_Level_Defn
top_level_proc = do
    proc_name <- try (identifier <* char '(')
    return =<< (try (top_level_func proc_name) <|> top_level_sub proc_name)

top_level_func :: Identifier -> Parser Top_Level_Defn
top_level_func func_name = do
    param <- pattern <* char ')' <* spaces <* char '=' <* spaces
    short_top_level_func func_name param <|> long_top_level_func func_name param

short_top_level_func :: Identifier -> Param -> Parser Top_Level_Defn
short_top_level_func func_name param = do
    body <- raw_expr
    return $ ShortFuncDefn func_name param body

long_top_level_func :: Identifier -> Param -> Parser Top_Level_Defn
long_top_level_func func_name param = do
    stmts <- string "do" *> endline *> stmt_block
    optional_end_name func_name
    return $ FuncDefn func_name param stmts

top_level_sub :: Identifier -> Parser Top_Level_Defn
top_level_sub sub_name = do
    param <- optionMaybe pattern <* char ')' <* spaces <* char '=' <* spaces <* string "do" <* endline
    stmts <- stmt_block
    optional_end_name sub_name
    return $ SubDefn sub_name param stmts
