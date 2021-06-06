module TestParser where

import qualified Data.Text.IO as Text
import System.Directory
import System.Exit (exitFailure, exitSuccess)
import System.IO

import Main_Parser (
    parse_souc_file,
    pretty_print,
    render_error,
    render_file_contents
    )

parse_valid_dir :: String
parse_valid_dir = "test/parser.d/valid/"

parse_invalid_dir :: String
parse_invalid_dir = "test/parser.d/invalid/"


main :: IO ()
main = do
    putStr "=== testing parser... "
    valid_files <- listDirectory parse_valid_dir
    mapM_ test_valid_file (map (parse_valid_dir <>) valid_files)
    putStrLn "=== all valid tests passed. testing bad parses..."
    invalid_files <- listDirectory parse_invalid_dir
    mapM_ test_invalid_file (map (parse_invalid_dir <>) invalid_files)


test_valid_file :: FilePath -> IO ()
test_valid_file file = do
    putStr file
    putStr "... "
    input <- Text.readFile file
    case parse_souc_file file input of
        Right _ -> putStrLn "OK."
        Left err -> do
            putStrLn "FAILED: "
            Text.putStrLn (render_error err input)
            exitFailure


test_invalid_file :: FilePath -> IO ()
test_invalid_file file = do
    putStr file
    putStr "... "
    input <- Text.readFile file
    case parse_souc_file file input of
        Left _ -> putStrLn "OK."
        Right prog -> do
            putStrLn "FAILED (this file should not have parsed):"
            putStrLn "-----------------------------------------"
            pretty_print prog
            Text.putStrLn (render_file_contents input)
            exitFailure
