module Main_TypeChecker where

import Data.Foldable (traverse_)
import Data.List (sort)
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Common
import Imports.Parser (parse_module_header)
import Parser.SouCParser (runSouCParser)
import TypeChecker.TypeChecker (type_check)

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then do
            contents <- getContents
            putStrLn "no file name provided, reading from stdin."
            test "stdin" contents output_results
        else if head args == "--test"
            then run_default_test_suite
            else process_args args


process_args :: [String] -> IO ()
process_args = traverse_ process_arg

process_arg :: String -> IO ()
process_arg arg = do
    is_dir <- doesDirectoryExist arg
    if is_dir
        then process_dir output_results arg
        else process_file output_results arg

process_dir :: Handler -> FilePath -> IO ()
process_dir handler dir = do
    cwd <- getCurrentDirectory
    ls <- sort <$> listDirectory dir
    setCurrentDirectory dir
    traverse_ (process_file handler) ls
    setCurrentDirectory cwd

process_file :: Handler -> FilePath -> IO ()
process_file handler name = do
    contents <- readFile name
    test name contents handler

test :: FilePath -> String -> Handler -> IO ()
test filename input handle_parsed = do
    putStr filename
    putStr "... "
    case parse_module_header filename input of
        Left err -> putStrLn (show err) >> exitFailure
        Right (m, i, rest) -> case runSouCParser filename m i rest of
            Left err -> putStrLn (show err) >> exitFailure
            Right parsed -> handle_parsed parsed

run_default_test_suite :: IO ()
run_default_test_suite = do
    putStrLn "=== checking valid inputs ==="
    process_dir handle_prog_ok "test/typechecker.d/valid/"
    putStrLn "=== checking invalid inputs ==="
    process_dir handle_prog_bad "test/typechecker.d/invalid/"

type Handler = ParseTree -> IO ()

output_results :: Handler
output_results prog = case type_check prog of
    Left typecheck_error -> print typecheck_error >> exitFailure
    Right ok -> putStrLn "OK." >> print ok

handle_prog_ok :: Handler
handle_prog_ok prog = case type_check prog of
    Left typecheck_error -> print typecheck_error >> exitFailure
    Right _ -> putStrLn "OK."

handle_prog_bad :: Handler
handle_prog_bad prog = case type_check prog of
    Left _ -> putStrLn "OK."
    Right _ -> putStrLn "ERROR: should have failed type-check" >> exitFailure
