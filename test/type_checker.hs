{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.Context
import TypeChecker.Expressions
import Common

import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)

newtype Expected = Result (Either TypeError ())

type Test = (LocalScope, ExprTree, Text, Expected, String)

match :: Expected
match = Result (Right ())

mismatch :: Text -> Text -> Expected
mismatch x y = Result $ Left $ TypeMismatch (SoucType x (SoucKind 0)) (SoucType y (SoucKind 0))

no_exports_ctx :: ExportList
no_exports_ctx = ExportList []

empty_ctx :: LocalScope
empty_ctx = GlobalScope [] (ExportList [])

globals :: LocalScope
globals = GlobalScope [
    Bound (Identifier "x") SoucInteger,
    Bound (Identifier "s") SoucString,
    Bound (Identifier "c") SoucChar,
    Bound (Identifier "b") SoucBool
    ] no_exports_ctx

scoped :: LocalScope
scoped = InnerScope [
    BoundLocal (Identifier "x") SoucInteger Immut,
    BoundLocal (Identifier "s") SoucString Immut,
    BoundLocal (Identifier "c") SoucChar Immut,
    BoundLocal (Identifier "b") SoucBool Immut
    ] empty_ctx

tests :: [Test]
tests = [
    (empty_ctx, Leaf (LitInt 3), "Integer", match, "int"),
    (empty_ctx, Leaf (LitChar 'a'), "Char", match, "char"),
    (empty_ctx, Leaf (LitString "what"), "String", match, "string"),
    (empty_ctx, Leaf (Constructor "True"), "Bool", match, "bool"),
    (empty_ctx, Branch Plus (Leaf (LitInt 1)) (Leaf (LitInt 2)), "Integer", match, "plus"),
    (globals, Leaf (Var "x"), "Integer", match, "intvar"),
    (globals, Leaf (Var "s"), "String", match, "stringvar"),
    (globals, Leaf (Var "c"), "Char", match, "charvar"),
    (globals, Leaf (Var "b"), "Bool", match, "boolvar"),
    (globals, Signed (Leaf (Var "x")) SoucInteger, "Integer", match, "intvar2"),
    (globals, Signed (Leaf (Var "s")) SoucString, "String",  match, "stringvar2"),
    (globals, Signed (Leaf (Var "c")) SoucChar,  "Char", match, "charvar2"),
    (globals, Signed (Leaf (Var "b")) SoucBool,  "Bool", match, "boolvar2"),
    (globals, Signed (Twig Negate (Leaf (Var "b"))) SoucBool,  "Bool",     match, "negate"),
    (globals, Branch Plus (Leaf (Var "x")) (Leaf (Var "x")), "Integer", match, "plus2"),
    (globals, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) SoucInteger) SoucInteger) SoucInteger) SoucInteger) SoucInteger, "Integer", match, "long int"),
    (scoped, Leaf (Var "x"), "Integer", match, "scoped intvar"),
    (scoped, Leaf (Var "s"), "String", match, "scoped stringvar"),
    (scoped, Leaf (Var "c"), "Char", match, "scoped charvar"),
    (scoped, Leaf (Var "b"), "Bool", match, "scoped boolvar"),
    (scoped, Signed (Leaf (Var "x")) SoucInteger,   "Integer", match, "scoped intvar2"),
    (scoped, Signed (Leaf (Var "s")) SoucString, "String",  match, "scoped stringvar2"),
    (scoped, Signed (Leaf (Var "c")) SoucChar,  "Char", match, "scoped charvar2"),
    (scoped, Signed (Leaf (Var "b")) SoucBool,  "Bool", match, "scoped boolvar2"),
    (scoped, Twig Negate (Leaf (Var "b")),  "Bool", match, "scoped negate"),
    (scoped, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) SoucInteger) SoucInteger) SoucInteger) SoucInteger) SoucInteger, "Integer", match, "scoped long int")
    ]

borked_tests :: [Test]
borked_tests = [
    (empty_ctx, Leaf (LitInt 3), "Char", mismatch "Char" "Integer", "int"),
    (empty_ctx, Leaf (LitChar 'a'), "Integer", mismatch "Integer" "Char", "char"),
    (empty_ctx, Leaf (LitString "what"), "Char", mismatch "Char" "String", "string"),
    (empty_ctx, Leaf (Constructor "True"), "Char", mismatch "Char" "Bool", "bool"),
    (empty_ctx, Twig Negate (Leaf (LitChar 'a')), "Bork", mismatch "Bool" "Char", "negate"),
    (empty_ctx, Twig Negate (Leaf (LitChar 'a')), "Bork", mismatch "Bool" "Char", "negate2"),
    (empty_ctx, Branch Plus (Leaf (LitChar 'a')) (Leaf (LitInt 2)), "Bork", mismatch "Integer" "Char", "plus1"),
    (empty_ctx, Branch Plus (Leaf (LitInt 2)) (Leaf (LitChar 'a')), "Bork", mismatch "Integer" "Char", "plus2"),
    (globals, Leaf (Var "x"), "Char", mismatch "Char" "Integer", "intvar"),
    (globals, Leaf (Var "s"), "Bool", mismatch "Bool" "String", "stringvar"),
    (globals, Leaf (Var "c"), "String", mismatch "String" "Char", "charvar"),
    (globals, Leaf (Var "b"), "Char", mismatch "Char" "Bool", "boolvar"),
    (globals, Signed (Leaf (Var "x")) SoucBool, "Bool", mismatch "Bool" "Integer", "invalid lit"),
    (globals, Signed (Leaf (Var "x")) SoucBool, "Integer", mismatch "Bool" "Integer", "invalid lit 2"),
    (globals, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) SoucInteger) SoucInteger) SoucBool) SoucInteger) SoucInteger, "Integer", mismatch "Bool" "Integer", "long int"),
    (scoped, Leaf (Var "x"), "Char", mismatch "Char" "Integer", "scoped intvar"),
    (scoped, Leaf (Var "s"), "Bool", mismatch "Bool" "String", "scoped stringvar"),
    (scoped, Leaf (Var "c"), "String", mismatch "String" "Char", "scoped charvar"),
    (scoped, Leaf (Var "b"), "Char", mismatch "Char" "Bool", "scoped boolvar"),
    (scoped, Signed (Leaf (Var "x")) SoucBool, "Bool", mismatch "Bool" "Integer", "scoped invalid lit"),
    (scoped, Signed (Leaf (Var "x")) SoucBool, "Integer", mismatch "Bool" "Integer", "scoped invalid lit 2"),
    (scoped, Signed (Leaf (Var "x")) SoucBool, "Integer", mismatch "Bool" "Integer", "scoped invalid lit 2"),
    (scoped, Branch Plus (Leaf (LitInt 2)) (Leaf (Var "s")), "Bork", mismatch "Integer" "String", "plus3"),
    (scoped, Signed (Signed (Signed (Signed (Signed (Leaf (LitInt 42)) SoucInteger) SoucInteger) SoucBool) SoucInteger) SoucInteger, "Integer", mismatch "Bool" "Integer", "scoped long int")
    ]


main :: IO ()
main = do
    putStrLn "=== testing type-checker valid inputs"
    mapM_ test tests
    putStrLn "=== testing type-checker invalid inputs"
    mapM_ test borked_tests
    putStrLn "all type-checker tests passed :^)"


render :: Either TypeError () -> Text
render (Right ()) = "match"
render (Left (TypeMismatch (SoucType x _) (SoucType y _))) = "mismatch: " <> x <> " / " <> y
render (Left (MultipleDeclarations (Identifier i))) = "multiple declarations: " <> i
render (Left (Undeclared (Identifier i))) = "undeclared identifier " <> i
render _ = error "FIXME more complex types"

print_err :: Either TypeError () -> Either TypeError () -> IO ()
print_err expected actual = Text.putStrLn failmsg where
    failmsg = "FAILED! \n  expected " <> render expected <> " but got: " <> render actual

test :: Test -> IO ()
test (ctx, expr, expr_t, Result expected, name) = do
    putStr name >> putStr "... "
    let actual = check_expr ctx expr (SoucType expr_t (SoucKind 0))
    if expected == actual
        then putStrLn "OK."
        else print_err expected actual >> exitFailure
