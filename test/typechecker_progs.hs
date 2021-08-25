{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-} -- FIXME
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import Common

import Data.Text (Text)
import qualified Data.Text.IO as Text
import System.Exit (exitFailure)
import TextShow


type Test = (ParseTree, Either TypeError CheckedProgram, String)

tests = [
    (conster, conster_checked, "conster"),
    (int, int_checked, "int"),
    (borked, borked_checked, "borked"),
    (borked_import, borked_import_checked, "borked_import")
    ]

main :: IO ()
main = do
    putStrLn "=== testing type-checker trees"
    mapM_ test tests
    putStrLn "all type-checker tests passed :^)"

instance Eq CheckedProgram where
    CheckedProgram m0 i0 b0 == CheckedProgram m1 i1 b1 =
        m0 == m1 && i0 == i1 && b0 == b1

instance Eq SoucModule where
    SoucModule s0 _ == SoucModule s1 _ = s0 == s1

instance Eq ImportDecl where
    RelImport s0 == RelImport s1 = s0 == s1
    LibImport s0 == LibImport s1 = s0 == s1
    _ == _ = False

mismatch :: Text -> Text -> TypeError
mismatch x y = TypeMismatch (SoucType x (SoucKind 0)) (SoucType y (SoucKind 0))

render :: Either TypeError CheckedProgram -> Text
render (Right p) = showt p
render (Left (TypeMismatch (SoucType x _) (SoucType y _))) = "mismatch: " <> x <> " / " <> y
render (Left (MultipleDeclarations (Identifier i))) = "multiple declarations: " <> i
render (Left (Undeclared (Identifier i))) = "undeclared identifier " <> i
render _ = error "FIXME more complex types"

test :: Test -> IO ()
test (prog, expected, name) = do
    putStr name >> putStr "... "
--     print prog
    let actual = type_check prog
    if expected == actual
        then putStrLn "OK."
        else print_err expected actual >> exitFailure

print_err :: Either TypeError CheckedProgram -> Either TypeError CheckedProgram -> IO ()
print_err expected actual = Text.putStrLn failmsg where
    failmsg = "FAILED! \n expected:\n   " <> render expected <> "\n but got:\n   " <> render actual

program_header :: [Top_Level_Defn] -> ParseTree
program_header = ParseTree default_module [] -- no name, no imports

checked_program_header :: [Top_Level_Defn] -> CheckedProgram
checked_program_header = CheckedProgram default_module []

tests :: [Test]

notype :: Maybe SoucType
notype = Nothing

default_module :: SoucModule
default_module = SoucModule "anonymous_main_module" []

-- tests begin here

conster :: ParseTree
conster =  program_header [
    Top_Level_Const_Defn (Identifier "n") notype (Leaf (LitInt 42))]

conster_checked :: Either TypeError CheckedProgram
conster_checked = Right $ checked_program_header [
    Top_Level_Const_Defn (Identifier "n") notype (Leaf (LitInt 42))]

int :: ParseTree
int =  program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitInt 42))]

int_checked :: Either TypeError CheckedProgram
int_checked = Right $ checked_program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitInt 42))]

fn :: ParseTree
fn = program_header [
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) notype (Stmts [
        Stmt_Return (Just (Leaf (LitInt 42)))])]

fn_checked :: Either TypeError CheckedProgram
fn_checked = Right $ checked_program_header [
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) notype (Stmts [
        Stmt_Return (Just (Leaf (LitInt 42)))])]

fn2 :: ParseTree
fn2 = program_header [
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) (Just SoucInteger) (Stmts [
        Stmt_Return (Just (Leaf (LitInt 42)))])]

fn_checked2 :: Either TypeError CheckedProgram
fn_checked2 = Right $ checked_program_header [
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) (Just SoucInteger) (Stmts [
        Stmt_Return (Just (Leaf (LitInt 42)))])]

fn3 :: ParseTree
fn3 = program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitInt 42)),
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) notype (Stmts [
        Stmt_Return (Just (Leaf (Var "n")))])]

fn3_checked :: Either TypeError CheckedProgram
fn3_checked = Right $ checked_program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitInt 42)),
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) notype (Stmts [
        Stmt_Return (Just (Leaf (Var "n")))])]


borked :: ParseTree
borked = program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitChar 'a'))]

borked_checked :: Either TypeError CheckedProgram
borked_checked = Left (mismatch "Integer" "Char")

borked_fn :: ParseTree
borked_fn = program_header [
    FuncDefn (Identifier "f") (Param  (Identifier "x") notype) (Just SoucInteger) (Stmts [
        Stmt_Return (Just (Leaf (LitChar 'a')))])]

borked_fn_checked :: Either TypeError CheckedProgram
borked_fn_checked = Left $ mismatch "Integer" "Char"

borked_fn2 :: ParseTree
borked_fn2 = program_header [
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) (Just SoucInteger) (Stmts [
        Stmt_Return (Just (Leaf (LitChar 'a')))])]

borked_fn2_checked :: Either TypeError CheckedProgram
borked_fn2_checked = Left $ mismatch "Integer" "Char"

borked_fn3 :: ParseTree
borked_fn3 = program_header [
    FuncDefn (Identifier "f") (Param  (Identifier "x") (Just SoucInteger)) (Just SoucChar) (Stmts [
        Stmt_Return (Just (Leaf (Var "x")))])]

borked_fn3_checked :: Either TypeError CheckedProgram
borked_fn3_checked = Left $ mismatch "Char" "Integer"

borked_fn4 :: ParseTree
borked_fn4 = program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitInt 42)),
    FuncDefn (Identifier "f") (Param  (Identifier "x") notype) (Just SoucChar) (Stmts [
        Stmt_Return (Just (Leaf (Var "n")))])]

borked_fn4_checked :: Either TypeError CheckedProgram
borked_fn4_checked = Left $ mismatch "Char" "Integer"

subber_const_ass :: ParseTree
subber_const_ass = program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitInt 42)),
    SubDefn (Identifier "subby") Nothing notype (Stmts [
        Stmt_Const_Assign (Identifier "i") notype (Leaf (Var "n"))])]

subber_const_ass_checked :: Either TypeError CheckedProgram
subber_const_ass_checked = Left (mismatch "Integer" "Char")

borked_subber_const_ass :: ParseTree
borked_subber_const_ass = program_header [
    Top_Level_Const_Defn (Identifier "n") (Just SoucInteger) (Leaf (LitInt 42)),
    SubDefn (Identifier "subby") Nothing notype (Stmts [
        Stmt_Const_Assign (Identifier "x") notype (Leaf (Var "x"))])]

borked_subber_const_ass_checked :: Either TypeError CheckedProgram
borked_subber_const_ass_checked = Left (mismatch "Integer"  "Char")

borked_import :: ParseTree
borked_import = ParseTree default_module [LibImport "x"] [
    Top_Level_Const_Defn (Identifier "x") (Just SoucInteger) (Leaf (LitInt 42))]


borked_import_checked :: Either TypeError CheckedProgram
borked_import_checked = Left (MultipleDeclarations "x")
