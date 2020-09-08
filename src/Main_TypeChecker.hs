module Main_TypeChecker where

import Data.Foldable (traverse_)
import System.Directory
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Parser.SouCParser
import TypeChecker.TypeChecker

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then do
            contents <- getContents
            putStrLn "no file name provided, reading from stdin."
            test "stdin" contents
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
    test name contents


test :: FilePath -> String -> IO ()
test filename input = do
    putStr filename
    putStr "... "
    case runSouCParser filename input of
        Left err -> do
            putStrLn (show err)
            exitFailure
        Right prog_tree -> case type_check prog_tree of
            Left typecheck_error -> print typecheck_error >> exitFailure
            Right _ -> putStrLn "OK."
