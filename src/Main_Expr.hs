module Main_Expr where

import Control.Arrow ((|||))
import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Reader (runReader)
import Control.Monad.State (evalStateT)
import Control.Monad.Except (runExceptT)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Char (isSpace, ord)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine)

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import TextShow (showt)

import SurCC.Parser.ExprParser (parse_text_as_expr)
import SurCC.TypeChecker.Expressions (infer)
import SurCC.Common
import SurCC.Common.SoucTypes (SoucType)

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
           >>= traverse_ (Text.pack <&> pcepe <&> liftIO <&> (*> loop))

-- parse, check, eval, print expression
pcepe :: Text -> IO ()
pcepe line = unless (Text.all isSpace line) (print_expression line)

parse_stdin :: IO ()
parse_stdin = TextIO.getContents >>= parse_check_eval_print_exit

parse_all :: [Text] -> IO ()
parse_all exprs = mapM_ print_expression exprs


parse_check_eval_print_exit :: Text -> IO ()
parse_check_eval_print_exit = parse_check_eval <&> (fale ||| succeed)
    where
        fale txt = TextIO.putStrLn txt *> exitFailure
        succeed txt = TextIO.putStrLn txt *> exitSuccess

print_expression :: Text -> IO ()
print_expression =
    parse_check_eval <&> (TextIO.putStrLn ||| TextIO.putStrLn)


parse_check_eval :: Text -> Either Text Text
parse_check_eval input =
    case parse_text_as_expr input of
        Left err -> Left (show err & Text.pack)
        Right tree -> case type_check tree of
            Left err -> Left (showt err)
            Right t -> Right $
                (evaluate_astree tree & showt) <> ": " <> showt t


type_check :: ExprTree -> Either TypeError SoucType
type_check expr =
    runReader (evalStateT (runExceptT (infer expr)) mempty) mempty


-- this does some silly things to treat everything as an Integer
-- it should probably produce a Term instead?
evaluate_astree :: ExprTree -> Integer
evaluate_astree = \case
    Leaf t -> case t of
        Lit l -> case l of
            LitInt x -> x
            LitChar c -> fromIntegral (ord c)
            LitString s -> fromIntegral (Text.length s)
        Name "true" -> 1
        Name "false" -> 0
        Name "none" -> (-1)
        Name _ -> 42 -- should have failed to type-check anyway

    Signed e _ -> evaluate_astree e

    Twig op tree -> operate (evaluate_astree tree)
        where operate = case op of
                Deref -> (\n -> product [1..n]) -- factorial, lol
                GetAddr -> undefined
                Negate -> negate
                ToString -> undefined
                Pure -> error $ "can't evaluate pure"
                Join -> error $ "can't evaluate join"

    Branch op left right ->
        evaluate_astree left `operate` evaluate_astree right
            where operate = case op of
                    Plus   -> (+)
                    Minus  -> (-)
                    Splat  -> (*)
                    FieldDiv -> div -- FIXME
                    FloorDiv -> div
                    Hihat  -> (^)
                    Equals -> \x y -> if (x==y) then 1 else 0
                    GreaterThan -> \x y -> if (x>y) then 1 else 0
                    LesserThan -> \x y -> if (x<y) then 1 else 0
                    Combine  -> undefined
                    Apply  -> undefined -- definitely can't do this
                    whatever -> error $ "can't evaluate " ++ show whatever

    Match _expr _branches -> undefined -- cannot be input in 1 line anyway
