{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import TypeChecker.Context
import Common

import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import TextShow


instance Eq ExportList where
    ExportList b0 == ExportList b1 = b0 == b1

instance Eq LocalScope where
    GlobalScope b0 r0 == GlobalScope b1 r1 = b0 == b1 && r0 == r1
    InnerScope b0 r0 == InnerScope b1 r1 = b0 == b1 && r0 == r1
    _ == _ = False

instance TextShow BoundLocal where
    showb = \case
        (BoundLocal (Identifier i) t Mut) ->
            "Bound (mutable)" <> showb i <> ": " <> showb t
        (BoundLocal (Identifier i) t Immut) ->
            "Bound " <> showb i <> ": " <> showb t

instance TextShow ExportList where
    showb (ExportList binds) = "ExportList: " <> showb binds

instance TextShow LocalScope where
    showb = \case
        GlobalScope binds exports -> "GlobalScope " <> showb binds <> " , exports " <> showb exports
        InnerScope binds scope -> "InnerScope " <> showb binds <> " and more: " <> showb scope


no_exports_ctx :: ExportList
no_exports_ctx = ExportList []

type Test = ([ImportDecl], [TopLevelDefn], Either TypeError LocalScope, String)

tests :: [Test]
tests = [
    ([], [TopLevelConstDefn "i" (Just SoucInteger) (Leaf (LitInt 4))], Right (GlobalScope [bound_id "i" SoucInteger] no_exports_ctx), "int"),
    ([LibImport "salad", LibImport "tofu"], [], Right (GlobalScope [bound_id "salad" (SoucType "Module" (SoucKind 0)), bound_id "tofu" (SoucType "Module" (SoucKind 0))] no_exports_ctx), "imports")
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


render :: Either TypeError LocalScope -> Text
render (Right ctx) = showt ctx
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
