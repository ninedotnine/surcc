{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import TypeChecker.Context
import Common
import Builtins

import System.Exit (exitFailure)


instance Eq BuiltinsCtx where
    Builtins b0 == Builtins b1 = b0 == b1

instance Eq ExportList where
    ExportList b0 == ExportList b1 = b0 == b1

instance Eq LocalScope where
    GlobalScope b0 r0 == GlobalScope b1 r1 = b0 == b1 && r0 == r1
    InnerScope b0 r0 == InnerScope b1 r1 = b0 == b1 && r0 == r1
    _ == _ = False

no_exports_ctx :: ExportList
no_exports_ctx = ExportList []

type Test = ([ImportDecl], [Top_Level_Defn], Either TypeError LocalScope, String)

tests :: [Test]
tests = [
    ([], [Top_Level_Const_Defn "i" (Just (SoucType "Integer")) (Leaf (LitInt 4))], Right (GlobalScope [Bound "i" (SoucType "Integer")] no_exports_ctx), "int"),
    ([LibImport "salad", LibImport "tofu"], [], Right (GlobalScope [Bound "salad" (SoucType "Module"), Bound "tofu" (SoucType "Module")] no_exports_ctx), "imports")
    ]

borked_tests :: [Test]
borked_tests = [
    ([], [Top_Level_Const_Defn "c" (Just (SoucType "Integer")) (Leaf (LitChar 'a'))], Left (mismatch "Integer" "Char"), "bad char 0"),
    ([], [Top_Level_Const_Defn "b" Nothing (Signed (Leaf (LitChar 'a')) (SoucType "Bool"))], Left (mismatch "Bool" "Char"), "bad char 1"),
    ([], [Top_Level_Const_Defn "b" (Just (SoucType "Char")) (Signed (Leaf (LitChar 'a')) (SoucType "Bool"))], Left (mismatch "Bool" "Char"), "bad char 2")
    ]


main :: IO ()
main = do
    putStrLn "=== testing add_globals valid inputs"
    mapM_ test tests
    putStrLn "=== testing get-globals invalid inputs"
    mapM_ test borked_tests
    putStrLn "all type-checker tests passed :^)"


render :: Either TypeError LocalScope -> String
render (Right ctx) = show ctx
render (Left (TypeMismatch (SoucType x) (SoucType y))) = "mismatch: " <> x <> " / " <> y
render (Left (MultipleDeclarations (Identifier i))) = "multiple declarations for " <> i
render (Left (Undeclared (Identifier i))) = "undeclared identifier " <> i
render _ = error "FIXME more complex types"

mismatch :: String -> String -> TypeError
mismatch x y = TypeMismatch (SoucType x) (SoucType y)

print_err :: Either TypeError LocalScope -> Either TypeError LocalScope -> IO ()
print_err expected actual = putStrLn failmsg where
    failmsg = "FAILED! \n  expected " <> render expected <> " but got: " <> render actual

test :: Test -> IO ()
test (imps, stmts, expected, name) = do
    case add_imports imps no_exports_ctx of
        Left err -> putStr $ "FAILED (to add imports!?): " <> show err
        Right imports_ctx -> do
            putStr name >> putStr "... "
            let actual = add_globals imports_ctx stmts
            if expected == actual
                then putStrLn "OK."
                else print_err expected actual >> exitFailure
