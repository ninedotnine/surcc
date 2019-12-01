module Main_Expr where
import Data.Char (isSpace)
import Data.List (dropWhile, dropWhileEnd)
import Data.Functor ((<&>))
import Control.Monad (forever)
import System.IO (hFlush, stdout)

import ShuntingYard (print_shunting_yard)

main :: IO ()
main = do
    print_shunting_yard =<< getContents
    putChar '\n'
{-

main = forever $ do
    putStr "> "
    hFlush stdout
    input <- getLine
    print_shunting_yard input
-}
