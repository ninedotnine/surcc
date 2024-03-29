module Main_Parser (
    main,
    parse_surc_file,
    render_file_contents,
    render_error,
    pretty_print,
) where


import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Text.Parsec.Error (ParseError)
import TextShow (TextShow(..), printT)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import SurCC.Common
import SurCC.Imports.Parser (parse_module_header)
import SurCC.Parser.SurCParser (parse)

main :: IO ()
main = do
    (filename, input) <- getArgs >>= parse_args
    case parse_surc_file filename input of
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

parse_surc_file :: FilePath -> Text -> Either ParseError ParseTree
parse_surc_file filename input = do
    module_data <- parse_module_header filename input
    parse filename module_data

pretty_print :: ParseTree -> IO ()
pretty_print (ParseTree modul imps typedefs body) = do
    printT modul
    mapM_ printT imps
    mapM_ printT typedefs
    mapM_ prettyPrint body


prettyPrint :: TopLevelDefn -> IO ()
prettyPrint = Text.putStrLn <$> \case
    SubDefn name param (Just t) (Stmts stmts r) -> mconcat
        ["sub ", showt name, " returns (should be IO): ", showt t,
         " takes ", showt param, Text.unlines (prettifyStmt <$> stmts),
         " returns " <> showt r]
    SubDefn name param Nothing (Stmts stmts r) -> mconcat
        ["sub ", showt name, " takes ", showt param,
         " is ", (Text.unlines (map prettifyStmt stmts)),
         " returns " <> showt r]
    FuncDefn name param (Just t) (Stmts stmts r) -> mconcat
        ["fn ", showt name, " takn ", showt param, " returns: ",
         showt t, (Text.unlines (map ((" "<>) . prettifyStmt) stmts)),
         "returns " <> showt r]
    FuncDefn name param Nothing (Stmts stmts r) -> mconcat
        ["fn ", showt name, " takn ", showt param,
         " = ", (Text.unlines (map ((" "<>) . prettifyStmt) stmts)),
         "returns " <> showt r]
    ShortFuncDefn name param (Just t) expr -> mconcat
        ["fn ", showt name, " takn ", showt param,
         " returns: ", showt t, " = ", showt expr]
    ShortFuncDefn name param Nothing expr -> mconcat
        ["fn ", showt name, " takn ", showt param, " = ", showt expr]
    TopLevelConstDefn name (Just type_name) val -> mconcat
        ["const ", showt name, ": ", showt type_name, " = ", showt val]
    TopLevelConstDefn name Nothing val -> mconcat
        ["const ", showt name, " = ", showt val]
    MainDefn param (Just t) (Stmts stmts _r) -> mconcat
        ["main with ", showt param, " returns (IO?): ", showt t,
         " = ", Text.unlines (map ((" "<>) . prettifyStmt) stmts)]
    MainDefn param Nothing (Stmts stmts _r) -> mconcat
        ["main with  ", showt param,
         " = ", Text.unlines (map ((" "<>) . prettifyStmt) stmts)]

prettifyStmt :: Stmt -> Text
prettifyStmt stmt = showt stmt -- FIXME could be much prettier
