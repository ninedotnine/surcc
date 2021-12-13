module Main_Parser (
    main,
    parse_souc_file,
    render_file_contents,
    render_error,
    pretty_print,
) where


import Common
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Text.Parsec.Error (ParseError)
import TextShow (TextShow(..), printT)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Imports.Parser (parse_module_header)
import Parser.SouCParser (parse)

main :: IO ()
main = do
    (filename, input) <- getArgs >>= parse_args
    case parse_souc_file filename input of
        Left err -> do
            Text.putStr (render_error err input)
            exitFailure
        Right prog -> do
            pretty_print prog
            exitSuccess

parse_args :: [String] -> IO (FilePath, Text)
parse_args args = if length args < 1
    then do
        contents <- Text.getContents
        putStrLn "no file name provided, reading from stdin."
        pure ("stdin", contents)
    else let name = head args in do
        contents <- Text.readFile name
        pure (name, contents)

render_error :: ParseError -> Text -> Text
render_error err input =
    error_start <> Text.pack (show err) <> "\n" <> render_file_contents input
        where
          error_start =
            "-------------------- failed parse output:--------------------\n"

render_file_contents :: Text -> Text
render_file_contents text = header <> contents
    where
        header = "-------------------- file contents: --------------------\n"
        contents = Text.unlines $ zipWith (<>) numbers indented
        numbers = fmap (Text.pack . show) ([1..]::[Int])
        indented = fmap ("   "<>) (Text.lines (text))

parse_souc_file :: FilePath -> Text -> Either ParseError ParseTree
parse_souc_file filename input = do
    module_data <- parse_module_header filename input
    parse filename module_data

pretty_print :: ParseTree -> IO ()
pretty_print (ParseTree modul imps body) = do
    printT modul
    mapM_ printT imps
    mapM_ prettyPrint body


prettyPrint :: TopLevelDefn -> IO ()
prettyPrint = Text.putStrLn <$> \case
    SubDefn name param (Just t) (Stmts stmts) -> mconcat
        ["sub ", showt name, " returns (should be IO): ", showt t,
         " takes ", showt param, Text.unlines (prettifyStmt <$> stmts)]
    SubDefn name param Nothing (Stmts stmts) -> mconcat
        ["sub ", showt name, " takes ", showt param,
         " is ", (Text.unlines (map prettifyStmt stmts))]
    FuncDefn name param (Just t) (Stmts stmts) -> mconcat
        ["fn ", showt name, " takn ", showt param, " returns: ",
         showt t, (Text.unlines (map ((" "<>) . prettifyStmt) stmts))]
    FuncDefn name param Nothing (Stmts stmts) -> mconcat
        ["fn ", showt name, " takn ", showt param,
         " = ", (Text.unlines (map ((" "<>) . prettifyStmt) stmts))]
    ShortFuncDefn name param (Just t) expr -> mconcat
        ["fn ", showt name, " takn ", showt param,
         " returns: ", showt t, " = ", showt expr]
    ShortFuncDefn name param Nothing expr -> mconcat
        ["fn ", showt name, " takn ", showt param, " = ", showt expr]
    TopLevelConstDefn name (Just type_name) val -> mconcat
        ["const ", showt name, ": ", showt type_name, " = ", showt val]
    TopLevelConstDefn name Nothing val -> mconcat
        ["const ", showt name, " = ", showt val]
    MainDefn param (Just t) (Stmts stmts) -> mconcat
        ["main with ", showt param, " returns (IO?): ", showt t,
         " = ", Text.unlines (map ((" "<>) . prettifyStmt) stmts)]
    MainDefn param Nothing (Stmts stmts) -> mconcat
        ["main with  ", showt param,
         " = ", Text.unlines (map ((" "<>) . prettifyStmt) stmts)]

prettifyStmt :: Stmt -> Text
prettifyStmt stmt = showt stmt -- FIXME could be much prettier
