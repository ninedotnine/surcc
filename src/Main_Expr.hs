module Main_Expr where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalStateT)
import Control.Monad.Except (runExceptT)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Char (isSpace, ord)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine)

import Data.Text (Text)
import Data.Text qualified as Text

import SurCC.Parser.ExprParser (parse_text_as_expr)
import SurCC.Parser.Expr.Types (RawExpr(..))
import SurCC.TypeChecker.Expressions (infer)
import SurCC.TypeChecker.Context (Checker)
-- import SurCC.Common (TypeError, ExprTree(..), Term(..), Literal(..))
import SurCC.Common

main :: IO ()
main = do
    args <- getArgs
    case args of
        []    -> repl
        ["-"] -> parse_stdin
        _     -> parse_all (Text.pack <$> args)

repl :: IO ()
repl = runInputT defaultSettings loop where
    loop = getInputLine "expr> "
           >>= traverse_ (Text.pack <&> pepe <&> liftIO <&> (*> loop))

pepe :: Text -> IO ()
pepe line = unless (Text.all isSpace line) (parse_eval_print_expression line)

parse_stdin :: IO ()
parse_stdin = do
    input <- getContents
    case parse_text_as_expr (Text.pack input) of
        Left err -> putStrLn (show err) >> exitFailure
        Right tree -> putStrLn (eval_show_astree tree) >> exitSuccess

parse_all :: [Text] -> IO ()
parse_all exprs = mapM_ parse_eval_print_expression exprs


-----

parse_eval_print_expression :: Text -> IO ()
parse_eval_print_expression input =
    case parse_text_as_expr input of
        Left err -> putStrLn (show err)
        Right tree -> putStrLn (eval_show_astree tree)


run_expr_checker :: Checker a -> Either TypeError a
run_expr_checker checker =
    runReader (evalStateT (runExceptT checker) mempty) mempty


evaluate_astree :: ExprTree -> Integer
evaluate_astree (Leaf t) = case t of
    Lit l -> case l of
        LitInt x -> x
        LitChar c -> fromIntegral (ord c)
        LitString s -> fromIntegral (Text.length s)
    Name _ -> 42 -- all identifiers are bound to this, sure

evaluate_astree (Signed e _) = evaluate_astree e
evaluate_astree (Twig op tree) = operate (evaluate_astree tree)
    where operate = case op of
            Deref -> (\n -> product [1..n]) -- factorial, just for testing
            GetAddr -> undefined
            Negate -> negate
            ToString -> undefined
            Pure -> error $ "can't evaluate pure"
            Join -> error $ "can't evaluate join"

evaluate_astree (Branch op left right) = evaluate_astree left `operate` evaluate_astree right
    where operate = case op of
            Plus   -> (+)
            Minus  -> (-)
            Splat  -> (*)
            FieldDiv -> div -- FIXME
            FloorDiv -> div
            Modulo -> mod
            Hihat  -> (^)
            Equals -> undefined -- can't do this on integers
            GreaterThan -> undefined -- can't do this on integers
            LesserThan -> undefined -- can't do this on integers
            Combine  -> undefined
            Apply  -> undefined -- definitely can't do this
            whatever -> error $ "can't evaluate " ++ show whatever

evaluate_astree (Match _expr _branches) = undefined

eval_show_astree :: ExprTree -> String
eval_show_astree = evaluate_astree <&> show
