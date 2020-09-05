module Main_TypeChecker where

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Parser.SouCParser
import TypeChecker.TypeChecker

getFileData :: IO (FilePath, String)
getFileData = getArgs >>= \args -> if length args < 1
    then do
        contents <- getContents
        putStrLn "no file name provided, reading from stdin."
        return ("stdin", contents)
    else let name = head args in do
        contents <- readFile name
        return (name, contents)


main :: IO ()
main = do
    (filename, input) <- getFileData
    case runSouCParser filename input of
        Left err -> do
            putStrLn (show err)
            exitFailure
        Right prog_tree -> case type_check prog_tree of
            Left typecheck_error -> print typecheck_error >> exitFailure
            Right _ -> putStrLn ":^)"
