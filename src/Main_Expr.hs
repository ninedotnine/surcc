module Main_Expr where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Data.Char (isSpace)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine)

import Data.Text (Text)
import Data.Text qualified as Text

import SurCC.Parser.ExprParser

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        ["-"] -> parse_stdin
        _     -> parse_all (Text.pack <$> args)

repl :: IO ()
repl = runInputT defaultSettings loop where
    loop = do
        m_input <- getInputLine "expr> "
        case m_input of
            Nothing -> pure ()
            Just input -> liftIO (pepe (Text.pack input)) >> loop

pepe :: Text -> IO ()
pepe line = unless (Text.all isSpace line) (parse_eval_print_expression line)

parse_stdin :: IO ()
parse_stdin = do
    input <- getContents
    case parse_expression (RawExpr (Text.pack input)) of
        Left err -> putStrLn (show err) >> exitFailure
        Right tree -> putStrLn (eval_show_astree tree) >> exitSuccess

parse_all :: [Text] -> IO ()
parse_all exprs = mapM_ parse_eval_print_expression exprs
