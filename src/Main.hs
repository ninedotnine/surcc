module Main where

import SouC_Types
-- import Text.Parsec.String (parseFromFile)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Parser

main :: IO ()
main = putStrLn "main"
