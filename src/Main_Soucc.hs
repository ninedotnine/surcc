module Main_Soucc where

import Common
import CodeGen.CodeGen (generate)
import Imports.Parser (parse_module_header)
import Parser.SouCParser (runSouCParser)
import TypeChecker.TypeChecker (type_check)

-- import Text.Parsec.String (parseFromFile)
import qualified Data.Text.IO as Text
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    file_name <- getArgs >>= sanitize_args
    file_contents <- Text.readFile file_name
    case parse_module_header file_name file_contents of
        Left parse_error -> print parse_error >> exitFailure
        Right (modul, imports, rest) -> case runSouCParser file_name modul imports rest of
            Left parse_error -> print parse_error >> exitFailure
            Right prog_tree -> case type_check prog_tree of
                Left typecheck_error -> print typecheck_error >> exitFailure
                Right checked_prog -> Text.putStrLn (generate checked_prog)

sanitize_args :: [String] -> IO String
sanitize_args [] = putStrLn "no filename provided." >> exitFailure
sanitize_args (x:[]) = pure x
sanitize_args _ = putStrLn "too many args." >> exitFailure
