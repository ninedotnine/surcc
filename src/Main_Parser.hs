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
import Parser.SouCParser (runSouCParser)

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
render_error err input =  error_start <> Text.pack (show err) <> "\n" <> render_file_contents input

render_file_contents :: Text -> Text
render_file_contents text = header <> contents
    where
        header = "-------------------- file contents: --------------------\n"
        contents = Text.unlines $ zipWith (<>) numbers indented
        numbers = fmap (Text.pack . show) ([1..]::[Int])
        indented = fmap ("   "<>) (Text.lines (text))

parse_souc_file :: FilePath -> Text -> Either ParseError ParseTree
parse_souc_file filename input = do
    (modul, imports, rest) <- parse_module_header filename input
    runSouCParser filename modul imports rest

pretty_print :: ParseTree -> IO ()
pretty_print (ParseTree modul imps body) = do
    print modul
    mapM_ print imps
    mapM_ prettyPrint body


prettyPrint :: Top_Level_Defn -> IO ()
prettyPrint (SubDefn name param (Just t) (Stmts stmts)) = do
    putStr $ "sub " ++ show name ++ " returns (should be IO): " ++ show t ++ " takes "
    putStrLn $ show param
    putStrLn (unlines (map prettifyStmt stmts))
prettyPrint (SubDefn name param Nothing (Stmts stmts)) = do
    putStr $ "sub " ++ show name ++ " takes "
    putStrLn $ show param
    putStrLn (unlines (map prettifyStmt stmts))
prettyPrint (FuncDefn name param (Just t) (Stmts stmts)) = do
    putStrLn $ "fn " ++ show name ++ " takn " ++ show param ++ " returns: " ++ show t ++ (
                unlines $  (map ((' ':) . prettifyStmt) stmts))
prettyPrint (FuncDefn name param Nothing (Stmts stmts)) = do
    putStrLn $ "fn " ++ show name ++ " takn " ++ show param ++ " " ++ (
                unlines $  (map ((' ':) . prettifyStmt) stmts))
prettyPrint (ShortFuncDefn name param (Just t) expr) = do
    putStrLn $ "fn" ++ show name ++ " takn " ++ show param ++ " returns: " ++ show t ++ " = " ++ show expr
prettyPrint (ShortFuncDefn name param Nothing expr) = do
    putStrLn $ "fn" ++ show name ++ " takn " ++ show param ++ " = " ++ show expr
prettyPrint (Top_Level_Const_Defn name (Just type_name) val) = do
    putStrLn $ "const " ++ show name ++ ": " ++ show type_name ++ " = " ++ show val
prettyPrint (Top_Level_Const_Defn name Nothing val) = do
    putStrLn $ "const " ++ show name ++ " = " ++ show val
prettyPrint (MainDefn param (Just t) (Stmts stmts)) = do
    putStrLn $ "main with args? " ++ show param ++ " returns (IO?): " ++ show t ++ " = "
    putStrLn $ unlines (map ((' ':) . prettifyStmt) stmts)
prettyPrint (MainDefn param Nothing (Stmts stmts)) = do
    putStrLn $ "main with args? " ++ show param ++ " = "
    putStrLn $ unlines (map ((' ':) . prettifyStmt) stmts)

prettifyStmt :: Stmt -> String
prettifyStmt stmt = show stmt -- FIXME could be much prettier

error_start = "-------------------- failed parse output:--------------------\n"
