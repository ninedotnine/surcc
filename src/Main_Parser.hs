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
    print modul
    mapM_ print imps
    mapM_ prettyPrint body


prettyPrint :: Top_Level_Defn -> IO ()
prettyPrint = \case
    SubDefn name param (Just t) (Stmts stmts) -> putStrLn $
        "sub " ++ show name ++ " returns (should be IO): " ++ show t ++
        " takes " ++ show param ++ (unlines (map prettifyStmt stmts))
    SubDefn name param Nothing (Stmts stmts) -> putStrLn $
        "sub " ++ show name ++ " takes " ++ show param ++
        " is " ++ (unlines (map prettifyStmt stmts))
    FuncDefn name param (Just t) (Stmts stmts) -> putStrLn $
        "fn " ++ show name ++ " takn " ++ show param ++
        " returns: " ++ show t ++ (unlines (map ((' ':) . prettifyStmt) stmts))
    FuncDefn name param Nothing (Stmts stmts) -> putStrLn $
        "fn " ++ show name ++ " takn " ++ show param ++
        " = " ++ (unlines (map ((' ':) . prettifyStmt) stmts))
    ShortFuncDefn name param (Just t) expr -> putStrLn $
        "fn " ++ show name ++ " takn " ++ show param ++
        " returns: " ++ show t ++ " = " ++ show expr
    ShortFuncDefn name param Nothing expr -> putStrLn $
        "fn " ++ show name ++ " takn " ++ show param ++ " = " ++ show expr
    Top_Level_Const_Defn name (Just type_name) val -> putStrLn $
        "const " ++ show name ++ ": " ++ show type_name ++ " = " ++ show val
    Top_Level_Const_Defn name Nothing val -> putStrLn $
        "const " ++ show name ++ " = " ++ show val
    MainDefn param (Just t) (Stmts stmts) -> putStrLn $
        "main with " ++ show param ++ " returns (IO?): " ++ show t ++
        " = " ++ unlines (map ((' ':) . prettifyStmt) stmts)
    MainDefn param Nothing (Stmts stmts) -> putStrLn $
        "main with  " ++ show param ++
        " = " ++ unlines (map ((' ':) . prettifyStmt) stmts)

prettifyStmt :: Stmt -> String
prettifyStmt stmt = show stmt -- FIXME could be much prettier
