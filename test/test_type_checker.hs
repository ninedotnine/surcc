{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-} -- this is crazy, FIXME maybe?
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import TypeChecker.Context
import Common

import System.Exit (exitFailure)
import Data.String (IsString(..))


newtype Expected = Result (Maybe TypeError)

type Test = (Context, ASTree, TypeName, Expected, String)

match :: Expected
match = Result Nothing

mismatch :: TypeName -> TypeName -> Expected
mismatch x y = Result $ Just $ TypeError x y

empty_ctx :: Context
empty_ctx = Global []

globals :: Context
globals = Global [
    Bound (Identifier "x") (TypeName "Int"),
    Bound (Identifier "s") (TypeName "String"),
    Bound (Identifier "c") (TypeName "Char"),
    Bound (Identifier "b") (TypeName "Bool")
    ]

scoped :: Context
scoped = Scoped [
    Bound (Identifier "x") (TypeName "Int"),
    Bound (Identifier "s") (TypeName "String"),
    Bound (Identifier "c") (TypeName "Char"),
    Bound (Identifier "b") (TypeName "Bool")
    ] empty_ctx

tests :: [Test]
tests = [
    (empty_ctx, Leaf (LitInt 3), "Integer", match, "int"),
    (empty_ctx, Leaf (LitChar 'a'), "Char", match, "char"),
    (empty_ctx, Leaf (LitString "what"), "String", match, "string"),
    (empty_ctx, Leaf (LitBool True), "Bool", match, "bool"),
    (globals, Leaf (Var "x"), "Int", match, "intvar"),
    (globals, Leaf (Var "s"), "String", match, "stringvar"),
    (globals, Leaf (Var "c"), "Char", match, "charvar"),
    (globals, Leaf (Var "b"), "Bool", match, "boolvar"),
    (globals, Signed (Leaf (Var "x")) "Int",   "Int",      match, "intvar2"),
    (globals, Signed (Leaf (Var "s")) "String", "String",  match, "stringvar2"),
    (globals, Signed (Leaf (Var "c")) "Char",  "Char",     match, "charvar2"),
    (globals, Signed (Leaf (Var "b")) "Bool",  "Bool",     match, "boolvar2"),
    (globals, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer"), "Integer", match, "long int"),
    (scoped, Leaf (Var "x"), "Int", match, "scoped intvar"),
    (scoped, Leaf (Var "s"), "String", match, "scoped stringvar"),
    (scoped, Leaf (Var "c"), "Char", match, "scoped charvar"),
    (scoped, Leaf (Var "b"), "Bool", match, "scoped boolvar"),
    (scoped, Signed (Leaf (Var "x")) "Int",   "Int",      match, "scoped intvar2"),
    (scoped, Signed (Leaf (Var "s")) "String", "String",  match, "scoped stringvar2"),
    (scoped, Signed (Leaf (Var "c")) "Char",  "Char",     match, "scoped charvar2"),
    (scoped, Signed (Leaf (Var "b")) "Bool",  "Bool",     match, "scoped boolvar2"),
    (scoped, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer"), "Integer", match, "scoped long int")
    ]

borked_tests :: [Test]
borked_tests = [
    (empty_ctx, Leaf (LitInt 3), "Char", mismatch "Char" "Integer", "int"),
    (empty_ctx, Leaf (LitChar 'a'), "Int", mismatch "Int" "Char", "char"),
    (empty_ctx, Leaf (LitString "what"), "Char", mismatch "Char" "String", "string"),
    (empty_ctx, Leaf (LitBool True), "Char", mismatch "Char" "Bool", "bool"),
    (globals, Leaf (Var "x"), "Char", mismatch "Char" "Int", "intvar"),
    (globals, Leaf (Var "s"), "Bool", mismatch "Bool" "String", "stringvar"),
    (globals, Leaf (Var "c"), "String", mismatch "String" "Char", "charvar"),
    (globals, Leaf (Var "b"), "Char", mismatch "Char" "Bool", "boolvar"),
    (globals, Signed (Leaf (Var "x")) "Bool", "Bool",   mismatch "Bool" "Int", "invalid lit"),
    (globals, Signed (Leaf (Var "x")) "Bool", "Int",    mismatch "Bool" "Int", "invalid lit 2"),
    (globals, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Bool")) (TypeName "Integer")) (TypeName "Integer"), "Integer", mismatch "Bool" "Integer", "long int"),
    (scoped, Leaf (Var "x"), "Char", mismatch "Char" "Int", "scoped intvar"),
    (scoped, Leaf (Var "s"), "Bool", mismatch "Bool" "String", "scoped stringvar"),
    (scoped, Leaf (Var "c"), "String", mismatch "String" "Char", "scoped charvar"),
    (scoped, Leaf (Var "b"), "Char", mismatch "Char" "Bool", "scoped boolvar"),
    (scoped, Signed (Leaf (Var "x")) "Bool", "Bool",   mismatch "Bool" "Int", "scoped invalid lit"),
    (scoped, Signed (Leaf (Var "x")) "Bool", "Int",    mismatch "Bool" "Int", "scoped invalid lit 2"),
    (scoped, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Bool")) (TypeName "Integer")) (TypeName "Integer"), "Integer", mismatch "Bool" "Integer", "scoped long int")
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

instance IsString Identifier where
    fromString = Identifier


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
