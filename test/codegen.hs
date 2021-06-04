-- this test only ensures that *something* is generated for each program tree.
{-# OPTIONS_GHC -Wall #-}

import CodeGen.CodeGen
import Common

import System.Directory (createDirectoryIfMissing)
import System.Process (callProcess)
-- import System.IO

default_module :: SoucModule
default_module = SoucModule "anonymous_main_module" []

tmpdir :: FilePath
tmpdir = "/tmp/souc_code_gen_test/"

test :: CheckedProgram -> String -> IO ()
test prog name = do
    putStr name >> putStr "... "
--     print prog
--     print (generate prog)
    let bin_filename = tmpdir ++ name
        c_filename = bin_filename ++ ".c"
    writeFile c_filename (generate prog)
    callProcess "gcc" [c_filename, "-o", bin_filename]
    putStrLn "OK."

main :: IO ()
main = do
    System.Directory.createDirectoryIfMissing True tmpdir
    putStrLn "=== testing codegen"
    run_tests
    putStrLn "all code gen tests passed :^)"

run_tests :: IO ()
run_tests = do
    test conster "conster"
    test func "func"
    test func2 "func2"
    test mainer "mainer"
    test mainer2 "mainer2"
    test subber "subber"
    test subber_ass "subber_ass"
    test subber_const_ass "subber_const_ass"
    test subber_postfix_oper "subber_postfix_oper" -- FIXME
    test subber_while "subber_while"
    test subber_if "subber_if"
    test subber_if_else "subber_if_else"
    test subber_unless "subber_unless"
    test subber_unless_else "subber_unless_else"
    test sub_while "sub_while"
    test sub_until "sub_until"

conster :: CheckedProgram
conster = CheckedProgram default_module [] [
    Top_Level_Const_Defn (Identifier "x") Nothing (Leaf (LitInt 42)),
    MainDefn Nothing Nothing (Stmts [
        Stmt_Const_Assign (Identifier "x2") Nothing (Leaf (LitInt 0))])]

func :: CheckedProgram
func = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    ShortFuncDefn (Identifier "f") (Param (Identifier "x") Nothing) Nothing (Leaf (LitInt 42))]

func2 :: CheckedProgram
func2 = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    FuncDefn (Identifier "f") (Param  (Identifier "x") Nothing) Nothing (Stmts [
        Stmt_Return (Just (Leaf (LitInt 42)))])]

mainer :: CheckedProgram
mainer = CheckedProgram default_module [] [MainDefn Nothing Nothing (Stmts [])]

mainer2 :: CheckedProgram
mainer2 = CheckedProgram default_module [] [MainDefn Nothing Nothing (Stmts [])]

subber :: CheckedProgram
subber = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts [
        Stmt_Sub_Call (Identifier "abort") Nothing])]

subber_ass :: CheckedProgram
subber_ass = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 42))])]

subber_const_ass :: CheckedProgram
subber_const_ass = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Const_Assign (Identifier "x") Nothing (Leaf (LitInt 42))])]

subber_postfix_oper :: CheckedProgram
subber_postfix_oper = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
        Stmt_Postfix_Oper (Identifier "x") "++"])]

subber_while :: CheckedProgram
subber_while = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_While (Leaf (Constructor "False")) (Stmts [
            Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
            Stmt_Postfix_Oper (Identifier "x") "++"])])]

subber_if :: CheckedProgram
subber_if = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_If (Leaf (Constructor "False")) (Stmts [
            Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
            Stmt_Postfix_Oper (Identifier "x") "++"])
            Nothing])]

subber_if_else :: CheckedProgram
subber_if_else = CheckedProgram default_module [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
        Stmt_If (Leaf (LitInt 0)) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"])
            (Just (Stmts [
                (Stmt_Postfix_Oper (Identifier "x") "--")]))]),
    MainDefn Nothing Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing])]

subber_unless :: CheckedProgram
subber_unless = CheckedProgram default_module [] [
    MainDefn Nothing Nothing (Stmts []),
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Unless (Leaf (Constructor "False")) (Stmts [
            Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
            Stmt_Postfix_Oper (Identifier "x") "++"])
            Nothing])]

subber_unless_else :: CheckedProgram
subber_unless_else = CheckedProgram default_module [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
        Stmt_Unless (Leaf (Constructor "False")) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"])
            (Just (Stmts [
                (Stmt_Postfix_Oper (Identifier "x") "--")]))]),
    MainDefn Nothing Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing])]

sub_while :: CheckedProgram
sub_while = CheckedProgram default_module [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
        Stmt_While (Leaf (Constructor "True")) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"])]),
    MainDefn Nothing Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing])]

sub_until :: CheckedProgram
sub_until = CheckedProgram default_module [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Assign (Identifier "x") Nothing (Leaf (LitInt 41)),
        Stmt_Until (Leaf (Constructor "False")) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"])]),
    MainDefn Nothing Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing])]
