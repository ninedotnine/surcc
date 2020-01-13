module Main_Expr where

import Control.Monad (forever, unless)
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)

import Parser.ExprParser

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        ["-"] -> parse_stdin
        _     -> parse_all args

repl :: IO ()
repl = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    unless (all isSpace input) (parse_eval_print_expression input)

parse_stdin :: IO ()
parse_stdin = do
    input <- getContents
    case parse_expression input of
        Left err -> putStrLn (show err) >> exitFailure
        Right tree -> putStrLn (eval_show_astree tree) >> exitSuccess

parse_all :: [String] -> IO ()
parse_all exprs = mapM_ parse_eval_print_expression exprs
