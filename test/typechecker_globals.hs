{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import TypeChecker.Context
import Common

import System.Exit (exitFailure)

instance Eq Bound where
    Bound i0 t0 == Bound i1 t1 = i0 == i1 && t0 == t1

instance Eq Context where
    Global b0 == Global b1 = b0 == b1
    Scoped b0 r0 == Scoped b1 r1 = b0 == b1 && r0 == r1
    _ == _ = False


type Test = ([Import], [Top_Level_Defn], Either TypeError Context, String)

tests :: [Test]
tests = [
    ([], [Top_Level_Const_Defn "i" (Just "Integer") (Leaf (LitInt 4))], Right (Global [Bound "i" (SoucType "Integer")]), "int"),
    ([Import "salad", Import "tofu"], [], Right (Global [Bound "salad" (SoucType "Module"), Bound "tofu" (SoucType "Module")]), "imports")
    ]

borked_tests :: [Test]
borked_tests = [
    ([], [Top_Level_Const_Defn "c" (Just "Integer") (Leaf (LitChar 'a'))], Left (mismatch "Integer" "Char"), "bad char 0"),
    ([], [Top_Level_Const_Defn "b" Nothing (Signed (Leaf (LitChar 'a')) "Bool")], Left (mismatch "Bool" "Char"), "bad char 1"),
    ([], [Top_Level_Const_Defn "b" (Just "Char") (Signed (Leaf (LitChar 'a')) "Bool")], Left (mismatch "Bool" "Char"), "bad char 2")
    ]


main :: IO ()
main = do
    putStrLn "=== testing add_globals valid inputs"
    mapM_ test tests
    putStrLn "=== testing get-globals invalid inputs"
    mapM_ test borked_tests
    putStrLn "all type-checker tests passed :^)"


render :: Either TypeError Context -> String
render (Right ctx) = show ctx
render (Left (TypeMismatch (SoucType (TypeName x)) (SoucType (TypeName y)))) = "mismatch: " <> x <> " / " <> y
render (Left (MultipleDeclarations (Identifier i))) = "multiple declarations for " <> i
render (Left (Undeclared (Identifier i))) = "undeclared identifier " <> i
render _ = error "FIXME more complex types"

mismatch :: TypeName -> TypeName -> TypeError
mismatch x y = TypeMismatch (SoucType x) (SoucType y)

print_err :: Either TypeError Context -> Either TypeError Context -> IO ()
print_err expected actual = putStrLn failmsg where
    failmsg = "FAILED! \n  expected " <> render expected <> " but got: " <> render actual

test :: Test -> IO ()
test (imps, stmts, expected, name) = do
    case add_imports imps of
        Left err -> putStr $ "FAILED (to add imports!?): " <> show err
        Right imports_ctx -> do
            putStr name >> putStr "... "
            let actual = add_globals imports_ctx stmts
            if expected == actual
                then putStrLn "OK."
                else print_err expected actual >> exitFailure
