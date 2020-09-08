module Main_TypeChecker where

import Data.Foldable (traverse_)
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Common
import Parser.SouCParser
import TypeChecker.TypeChecker

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then do
            contents <- getContents
            putStrLn "no file name provided, reading from stdin."
            test "stdin" contents handle_prog_ok
        else if
            head args == "--test"
            then run_default_test_suite
            else process_args args


process_args :: [String] -> IO ()
process_args = traverse_ process_arg

process_arg :: String -> IO ()
process_arg arg = do
    is_dir <- doesDirectoryExist arg
    if is_dir
        then process_dir arg
        else process_file arg

process_dir :: FilePath -> IO ()
process_dir dir = do
    cwd <- getCurrentDirectory
    ls <- listDirectory dir
    setCurrentDirectory dir
    traverse_ process_file ls
    setCurrentDirectory cwd

process_file :: FilePath -> IO ()
process_file name = do
    contents <- readFile name
    test name contents handle_prog_ok

test :: FilePath -> String -> (Program -> IO ()) -> IO ()
test filename input handle_parsed = do
    putStr filename
    putStr "... "
    case runSouCParser filename input of
        Left err -> do
            putStrLn (show err)
            exitFailure
        Right parsed -> handle_parsed parsed

handle_prog_ok :: Program -> IO ()
handle_prog_ok prog = case type_check prog of
    Left typecheck_error -> print typecheck_error >> exitFailure
    Right _ -> putStrLn "OK."

run_default_test_suite :: IO ()
run_default_test_suite = do
    putStrLn "=== checking valid inputs ==="
    process_dir "test/typechecker.d/valid/"
    putStrLn "=== checking invalid inputs ==="
    process_invalid_dir "test/typechecker.d/invalid/"

process_invalid_dir :: FilePath -> IO ()
process_invalid_dir dir = do
    cwd <- getCurrentDirectory
    ls <- listDirectory dir
    print ls
    setCurrentDirectory dir
    traverse_ process_invalid_file ls
    setCurrentDirectory cwd

process_invalid_file :: FilePath -> IO ()
process_invalid_file name = do
    contents <- readFile name
    test name contents handle_prog_bad where
        handle_prog_bad prog = case type_check prog of
            Left _ -> putStrLn "OK."
            Right _ -> putStrLn "ERROR: should have failed type-check" >> exitFailure
