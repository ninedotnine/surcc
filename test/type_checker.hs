{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.Context
import TypeChecker.Expressions
import Common

import System.Exit (exitFailure)

newtype Expected = Result (Either TypeError ())

type Test = (Context, ASTree, TypeName, Expected, String)

match :: Expected
match = Result (Right ())

mismatch :: TypeName -> TypeName -> Expected
mismatch x y = Result $ Left $ TypeMismatch (SoucType x) (SoucType y)

empty_ctx :: Context
empty_ctx = Global []

globals :: Context
globals = Global [
    Bound (Identifier "x") (SoucType "Integer"),
    Bound (Identifier "s") (SoucType "String"),
    Bound (Identifier "c") (SoucType "Char"),
    Bound (Identifier "b") (SoucType "Bool")
    ]

scoped :: Context
scoped = Scoped [
    Bound (Identifier "x") (SoucType "Integer"),
    Bound (Identifier "s") (SoucType "String"),
    Bound (Identifier "c") (SoucType "Char"),
    Bound (Identifier "b") (SoucType "Bool")
    ] empty_ctx

tests :: [Test]
tests = [
    (empty_ctx, Leaf (LitInt 3), "Integer", match, "int"),
    (empty_ctx, Leaf (LitChar 'a'), "Char", match, "char"),
    (empty_ctx, Leaf (LitString "what"), "String", match, "string"),
    (empty_ctx, Leaf (LitBool True), "Bool", match, "bool"),
    (empty_ctx, Branch Plus (Leaf (LitInt 1)) (Leaf (LitInt 2)), "Integer", match, "plus"),
    (globals, Leaf (Var "x"), "Integer", match, "intvar"),
    (globals, Leaf (Var "s"), "String", match, "stringvar"),
    (globals, Leaf (Var "c"), "Char", match, "charvar"),
    (globals, Leaf (Var "b"), "Bool", match, "boolvar"),
    (globals, Signed (Leaf (Var "x")) "Integer",   "Integer", match, "intvar2"),
    (globals, Signed (Leaf (Var "s")) "String", "String",  match, "stringvar2"),
    (globals, Signed (Leaf (Var "c")) "Char",  "Char", match, "charvar2"),
    (globals, Signed (Leaf (Var "b")) "Bool",  "Bool", match, "boolvar2"),
    (globals, Signed (Twig Negate (Leaf (Var "b"))) "Bool",  "Bool",     match, "negate"),
    (globals, Branch Plus (Leaf (Var "x")) (Leaf (Var "x")), "Integer", match, "plus2"),
    (globals, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer"), "Integer", match, "long int"),
    (scoped, Leaf (Var "x"), "Integer", match, "scoped intvar"),
    (scoped, Leaf (Var "s"), "String", match, "scoped stringvar"),
    (scoped, Leaf (Var "c"), "Char", match, "scoped charvar"),
    (scoped, Leaf (Var "b"), "Bool", match, "scoped boolvar"),
    (scoped, Signed (Leaf (Var "x")) "Integer",   "Integer", match, "scoped intvar2"),
    (scoped, Signed (Leaf (Var "s")) "String", "String",  match, "scoped stringvar2"),
    (scoped, Signed (Leaf (Var "c")) "Char",  "Char", match, "scoped charvar2"),
    (scoped, Signed (Leaf (Var "b")) "Bool",  "Bool", match, "scoped boolvar2"),
    (scoped, Twig Negate (Leaf (Var "b")),  "Bool", match, "scoped negate"),
    (scoped, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Integer"), "Integer", match, "scoped long int")
    ]

borked_tests :: [Test]
borked_tests = [
    (empty_ctx, Leaf (LitInt 3), "Char", mismatch "Char" "Integer", "int"),
    (empty_ctx, Leaf (LitChar 'a'), "Integer", mismatch "Integer" "Char", "char"),
    (empty_ctx, Leaf (LitString "what"), "Char", mismatch "Char" "String", "string"),
    (empty_ctx, Leaf (LitBool True), "Char", mismatch "Char" "Bool", "bool"),
    (empty_ctx, Twig Negate (Leaf (LitChar 'a')), "Bork", mismatch "Bool" "Char", "negate"),
    (empty_ctx, Twig Negate (Leaf (LitChar 'a')), "Bork", mismatch "Bool" "Char", "negate2"),
    (empty_ctx, Branch Plus (Leaf (LitChar 'a')) (Leaf (LitInt 2)), "Bork", mismatch "Integer" "Char", "plus1"),
    (empty_ctx, Branch Plus (Leaf (LitInt 2)) (Leaf (LitChar 'a')), "Bork", mismatch "Integer" "Char", "plus2"),
    (globals, Leaf (Var "x"), "Char", mismatch "Char" "Integer", "intvar"),
    (globals, Leaf (Var "s"), "Bool", mismatch "Bool" "String", "stringvar"),
    (globals, Leaf (Var "c"), "String", mismatch "String" "Char", "charvar"),
    (globals, Leaf (Var "b"), "Char", mismatch "Char" "Bool", "boolvar"),
    (globals, Signed (Leaf (Var "x")) "Bool", "Bool", mismatch "Bool" "Integer", "invalid lit"),
    (globals, Signed (Leaf (Var "x")) "Bool", "Integer", mismatch "Bool" "Integer", "invalid lit 2"),
    (globals, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Bool")) (TypeName "Integer")) (TypeName "Integer"), "Integer", mismatch "Bool" "Integer", "long int"),
    (scoped, Leaf (Var "x"), "Char", mismatch "Char" "Integer", "scoped intvar"),
    (scoped, Leaf (Var "s"), "Bool", mismatch "Bool" "String", "scoped stringvar"),
    (scoped, Leaf (Var "c"), "String", mismatch "String" "Char", "scoped charvar"),
    (scoped, Leaf (Var "b"), "Char", mismatch "Char" "Bool", "scoped boolvar"),
    (scoped, Signed (Leaf (Var "x")) "Bool", "Bool", mismatch "Bool" "Integer", "scoped invalid lit"),
    (scoped, Signed (Leaf (Var "x")) "Bool", "Integer", mismatch "Bool" "Integer", "scoped invalid lit 2"),
    (scoped, Signed (Leaf (Var "x")) "Bool", "Integer", mismatch "Bool" "Integer", "scoped invalid lit 2"),
    (scoped, Branch Plus (Leaf (LitInt 2)) (Leaf (Var "s")), "Bork", mismatch "Integer" "String", "plus3"),
    (scoped, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) (TypeName "Integer")) (TypeName "Integer")) (TypeName "Bool")) (TypeName "Integer")) (TypeName "Integer"), "Integer", mismatch "Bool" "Integer", "scoped long int")
    ]


main :: IO ()
main = do
    putStrLn "=== testing type-checker valid inputs"
    mapM_ test tests
    putStrLn "=== testing type-checker invalid inputs"
    mapM_ test borked_tests
    putStrLn "all type-checker tests passed :^)"


render :: Either TypeError () -> String
render (Right ()) = "match"
render (Left (TypeMismatch (SoucType (TypeName x)) (SoucType (TypeName y)))) = "mismatch: " <> x <> " / " <> y
render (Left (MultipleDeclarations (Identifier i))) = "multiple declarations: " <> i
render (Left (Undeclared (Identifier i))) = "undeclared identifier " <> i
render _ = error "FIXME more complex types"

print_err :: Either TypeError () -> Either TypeError () -> IO ()
print_err expected actual = putStrLn failmsg where
    failmsg = "FAILED! \n  expected " <> render expected <> " but got: " <> render actual

test :: Test -> IO ()
test (ctx, expr, expr_t, Result expected, name) = do
    putStr name >> putStr "... "
    let actual = check_astree ctx expr (SoucType expr_t)
    if expected == actual
        then putStrLn "OK."
        else print_err expected actual >> exitFailure
