{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- this is crazy, FIXME maybe?
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import TypeChecker.Context
import Common
import Parser.ExprParser (ASTree(..), Term(..))

import System.Exit (exitFailure)
import Data.String (IsString(..))

-- import System.IO


newtype Expected = Result (Maybe TypeError)

type Test = (Context, ASTree, TypeName, Expected, String)

match :: Expected
match = Result Nothing

tests :: [Test]
tests = [
    (Global [], Leaf (LitInt 3), "Integer", match, "int"),
    (Global [], Leaf (LitChar 'a'), "Char", match, "char"),
    (Global [], Leaf (LitString "what"), "String", match, "string"),
    (Global [], Leaf (LitBool True), "Bool", match, "bool")
    ]

mismatch :: TypeName -> TypeName -> Expected
mismatch x y = Result $ Just $ TypeError x y

borked_tests :: [Test]
borked_tests = [
    (Global [], Leaf (LitInt 3), "Char", mismatch "Char" "Integer", "int"),
    (Global [], Leaf (LitChar 'a'), "Int", mismatch "Int" "Char", "char"),
    (Global [], Leaf (LitString "what"), "Char", mismatch "Char" "String", "string"),
    (Global [], Leaf (LitBool True), "Char", mismatch "Char" "Bool", "bool")
    ]


main :: IO ()
main = do
    putStrLn "=== testing type-checker valid inputs"
    mapM_ test tests
    putStrLn "=== testing type-checker invalid inputs"
    mapM_ test borked_tests
    putStrLn "all type-checker tests passed :^)"



instance IsString TypeName where
    fromString = TypeName


render :: Maybe TypeError -> String
render Nothing = "match"
render (Just (TypeError (TypeName x) (TypeName y))) = "mismatch: " <> x <> " / " <> y

print_err :: Maybe TypeError -> Maybe TypeError -> IO ()
print_err expected actual = putStrLn failmsg where
    failmsg = "FAILED! \n  expected " <> render expected <> " but got: " <> render actual

test :: Test -> IO ()
test (ctx, expr, expr_t, Result expected, name) = do
    putStr name >> putStr "... "
    let actual = check_astree ctx expr expr_t
    if expected == actual
        then putStrLn "OK."
        else print_err expected actual >> exitFailure
