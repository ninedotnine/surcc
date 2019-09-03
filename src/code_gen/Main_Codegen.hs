module Main_Codegen where

import SouC_Types
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Parser

import CodeGen

get_parse_tree_from_file :: String -> IO Program
get_parse_tree_from_file name = do
    contents <- readFile name
    return (read contents)

main :: IO ()
main = do
    args <- getArgs
    if length args < 1
        then putStrLn "no arg provided." >> exitFailure
        else do
    let name = head args
    parsetree <- get_parse_tree_from_file name
    putStrLn name
    print parsetree
    print $ generate parsetree
