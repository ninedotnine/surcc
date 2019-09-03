module Main_Expr where
import Control.Monad (forever)
import System.IO (hFlush, stdout)
import Text.Parsec hiding (space, spaces, string)

import SouC_Expr

main :: IO ()
main = forever $ do
    input <- putStr "> " >> hFlush stdout >> getLine
    case runParser (raw_expr <* eof) 0 "raw_expr" input of
        Left err -> putStrLn $ "error: " ++ (show err)
        Right r -> print r
