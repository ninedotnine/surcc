module Main_Soucc where

import Common (TypeError)
import CodeGen.CodeGen (generate)
import Imports.Parser (parse_module_header)
import Parser.SouCParser (parse)
import TypeChecker.TypeChecker (type_check)

-- import Text.Parsec.String (parseFromFile)
import Control.Arrow (left)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Text.Parsec (SourceName, ParseError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    file_name <- getArgs >>= sanitize_args
    file_contents <- Text.readFile file_name
    output $ pipeline file_name file_contents

pipeline :: SourceName -> Text -> Either SouccError Text
pipeline name contents =
    left SouccHeaderError (parse_module_header name contents)
    >>= left SouccParseError . (parse name)
    >>= left SouccTypeError . type_check
    <&> generate

output :: Either SouccError Text -> IO ()
output = \case
    Left err -> print err >> exitFailure
    Right text -> Text.putStrLn text

sanitize_args :: [String] -> IO SourceName
sanitize_args = \case
    [] -> putStrLn "no filename provided." >> exitFailure
    (x:[]) -> pure x
    _ -> putStrLn "too many args." >> exitFailure

data SouccError = SouccHeaderError ParseError
                | SouccParseError ParseError
                | SouccTypeError TypeError

instance Show SouccError where
    show = \case
        SouccHeaderError err -> "invalid header:\n" ++ show err
        SouccParseError err -> "failed parse:\n" ++ show err
        SouccTypeError err -> "type error:\n" ++ show err
