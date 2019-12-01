module Main_Expr where
import Control.Monad (forever)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import ShuntingYard (print_shunting_yard)

main :: IO ()
main = do
    args <- getArgs
    if length args == 0
        then repl
        else if elem "-" args
            then print_shunting_yard =<< getContents
            else parse_all args

parse_once :: String -> IO ()
parse_once input = print_shunting_yard input

repl :: IO ()
repl = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    print_shunting_yard input

parse_all :: [String] -> IO ()
parse_all args = mapM_ print_shunting_yard args
