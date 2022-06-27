module Main_Surcc where

import SurCC.Common (TypeError)
import SurCC.CodeGen.CodeGen (generate)
import SurCC.Imports.Parser (parse_module_header)
import SurCC.Parser.SurCParser (parse)
import SurCC.TypeChecker.TypeChecker (type_check)

import Prelude hiding (putStr, putStrLn, readFile)
import Control.Arrow (left)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.IO (putStr, putStrLn, hPutStrLn, readFile)
import Text.Parsec (SourceName, ParseError)
import TextShow
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)

main :: IO ()
main = do
    file_name <- getArgs >>= sanitize_args
    file_contents <- readFile file_name
    output $ pipeline file_name file_contents

pipeline :: SourceName -> Text -> Either SouccError Text
pipeline name contents =
    left SouccHeaderError (parse_module_header name contents)
    >>= left SouccParseError . (parse name)
    >>= left SouccTypeError . type_check
    <&> generate

output :: Either SouccError Text -> IO ()
output = \case
    Left err -> hPutStrLn stderr (showt err) >> exitFailure
    Right text -> putStrLn text

sanitize_args :: [String] -> IO SourceName
sanitize_args = \case
    [] -> putStrLn "no filename provided." >> exitFailure
    (x:[]) -> pure x
    _ -> putStrLn "too many args." >> exitFailure

data SouccError = SouccHeaderError ParseError
                | SouccParseError ParseError
                | SouccTypeError TypeError

instance TextShow SouccError where
    showb = \case
        SouccHeaderError err -> "invalid header:\n" <> showb (show err)
        SouccParseError err -> "failed parse:\n" <> showb (show err)
        SouccTypeError err -> "type error:\n" <> showb err
