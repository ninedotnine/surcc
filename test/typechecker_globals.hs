{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import TypeChecker.Context
import Common

import Data.HashMap.Strict qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Text qualified as Text
import System.Exit (exitFailure)
import TextShow


instance Eq ExportList where
    ExportList b0 == ExportList b1 = b0 == b1

instance Eq LocalScope where
    GlobalScope b0 r0 == GlobalScope b1 r1 = b0 == b1 && r0 == r1
    InnerScope b0 r0 == InnerScope b1 r1 = b0 == b1 && r0 == r1
    _ == _ = False


no_exports_ctx :: ExportList
no_exports_ctx = ExportList Map.empty

type Test = ([ImportDecl], [TopLevelDefn], Either TypeError LocalScope, String)

tests :: [Test]
tests = [
    ([], [TopLevelConstDefn "i" (Just SoucInteger) (Leaf (LitInt 4))], Right (GlobalScope (Map.singleton (Left "i") SoucInteger) no_exports_ctx), "int"),
    ([LibImport "salad", LibImport "tofu"], [], Right (GlobalScope (Map.fromList [(Left "salad", SoucModuleType), (Left "tofu", SoucModuleType)]) no_exports_ctx), "imports")
    ]

borked_tests :: [Test]
borked_tests = [
    ([], [TopLevelConstDefn "c" (Just SoucInteger) (Leaf (LitChar 'a'))], Left (mismatch "Integer" "Char"), "bad char 0"),
    ([], [TopLevelConstDefn "b" Nothing (Signed (Leaf (LitChar 'a')) SoucBool)], Left (mismatch "Bool" "Char"), "bad char 1"),
    ([], [TopLevelConstDefn "b" (Just SoucChar) (Signed (Leaf (LitChar 'a')) SoucBool)], Left (mismatch "Bool" "Char"), "bad char 2")
    ]


main :: IO ()
main = do
    putStrLn "=== testing add_globals valid inputs"
    mapM_ test tests
    putStrLn "=== testing get-globals invalid inputs"
    mapM_ test borked_tests
    putStrLn "all type-checker tests passed :^)"

deriving instance Show LocalScope
deriving instance Show ExportList

render :: Either TypeError LocalScope -> Text
render (Right ctx) = Text.pack (show ctx)
render (Left (TypeMismatch (SoucType x _) (SoucType y _))) = "mismatch: " <> x <> " / " <> y
render (Left (MultipleDeclarations (Identifier i))) = "multiple declarations for " <> i
render (Left (Undeclared (Identifier i))) = "undeclared identifier " <> i
render _ = error "FIXME more complex types"

mismatch :: Text -> Text -> TypeError
mismatch x y = TypeMismatch (SoucType x (SoucKind 0)) (SoucType y (SoucKind 0))

print_err :: Either TypeError LocalScope -> Either TypeError LocalScope -> IO ()
print_err expected actual = Text.putStrLn failmsg where
    failmsg = "FAILED! \n  expected " <> render expected <> " but got: " <> render actual

test :: Test -> IO ()
test (imps, stmts, expected, name) = do
    case add_imports imps no_exports_ctx of
        Left err -> Text.putStr $ "FAILED (to add imports!?): " <> showt err
        Right imports_ctx -> do
            putStr name >> putStr "... "
            let actual = add_globals imports_ctx stmts
            if expected == actual
                then putStrLn "OK."
                else print_err expected actual >> exitFailure
