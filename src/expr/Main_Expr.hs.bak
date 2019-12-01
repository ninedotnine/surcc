module Main_Expr where
import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Text.Parsec hiding (space, spaces, string)

import SouC_Expr

main :: IO ()
main = forever $ do
    input <- putStr "> " >> hFlush stdout >> getLine
    putStrLn (run_raw_expr_parser input)
