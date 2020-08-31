module Main where

import Common
import CodeGen.CodeGen
import Parser.SouCParser
import TypeChecker.TypeChecker

-- import Text.Parsec.String (parseFromFile)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    file_name <- getArgs >>= sanitize_args
    file_contents <- readFile file_name
    case runSouCParser file_name file_contents of
        Left parse_error -> print parse_error >> exitFailure
        Right prog_tree -> case type_check prog_tree of
            Left typecheck_error -> print typecheck_error >> exitFailure
            Right checked_prog -> putStrLn (generate checked_prog)

sanitize_args :: [String] -> IO String
sanitize_args [] = putStrLn "no filename provided." >> exitFailure
sanitize_args (x:[]) = return x
sanitize_args _ = putStrLn "too many args." >> exitFailure
