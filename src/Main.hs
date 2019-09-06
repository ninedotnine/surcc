module Main where

import SouC_Types
import CodeGen
import Parser

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
        Right prog_tree -> putStrLn (generate prog_tree)

sanitize_args :: [String] -> IO String
sanitize_args [] = putStrLn "no filename provided." >> exitFailure
sanitize_args (x:[]) = return x
sanitize_args _ = putStrLn "too many args." >> exitFailure
