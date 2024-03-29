-- this test only ensures that *something* is generated for each program tree
-- and that gcc compiles it!

{-# OPTIONS_GHC -Wall #-}

import SurCC.CodeGen.CodeGen
import SurCC.Common

import Control.Monad (when)
import Data.Text.IO qualified as Text
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure, ExitCode(..))
import System.Process.Text (readProcessWithExitCode)

tmpdir :: FilePath
tmpdir = "/tmp/surcc_code_gen_test/"

test :: CheckedProgram -> String -> IO ()
test prog name = do
    putStr name >> putStr "... "
    let bin_filename = tmpdir ++ name
    (code, _, err) <- readProcessWithExitCode
        "gcc" ["-x", "c", "-o", bin_filename, "-"] (generate prog)
    when (code /= ExitSuccess) (
        Text.putStrLn "error" >> Text.putStrLn err >> debug_output (generate prog) >> exitFailure)
    putStrLn "OK."

-- debug_output :: IO
debug_output text = do
    Text.writeFile "/tmp/codegen_fail.c" text

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

default_main_param stdout = MainParam
    (MainParamStdIn False)
    (MainParamStdOut stdout)
    (MainParamStdErr False)
    (MainParamProgName False)
    (MainParamArgs False)
    (MainParamEnv False)

default_module :: SurCModule
default_module = SurCModule "anonymous_main_module" []

default_main :: TopLevelDefn
default_main = MainDefn (default_main_param True) Nothing short_stmts

short_stmts :: Stmts
short_stmts = Stmts [
    Stmt_Sub_Call (Identifier "write")
                  (Just (Branch Comma (Leaf (Name (Identifier "stdout")))
                                      (Leaf (Lit (LitString "hello")))))
    ] Nothing

conster :: CheckedProgram
conster = CheckedProgram default_module [] [] [
    TopLevelConstDefn (Identifier "x") Nothing (Leaf (Lit (LitInt 42))),
    MainDefn (default_main_param False) Nothing (Stmts [
        Stmt_Const_Assign_Static (Identifier "x2") Nothing (Leaf (Lit (LitInt 0)))] Nothing)]

func :: CheckedProgram
func = CheckedProgram default_module [] [] [
    default_main,
    ShortFuncDefn (Identifier "f") (Param (Identifier "x") Nothing) Nothing (Leaf (Lit (LitInt 42)))]

func2 :: CheckedProgram
func2 = CheckedProgram default_module [] [] [
    default_main,
    FuncDefn (Identifier "f") (Param (Identifier "x") Nothing)
        Nothing (Stmts [] (Just (Return (Just (Leaf (Lit (LitInt 42)))))))]


subber :: CheckedProgram
subber = CheckedProgram default_module [] [] [
    MainDefn (default_main_param False) Nothing (Stmts [
        Stmt_Sub_Call (Identifier "abort") Nothing] Nothing)]

subber_ass :: CheckedProgram
subber_ass = CheckedProgram default_module [] [] [
    default_main,
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 42)))
    ] Nothing)]

subber_const_ass :: CheckedProgram
subber_const_ass = CheckedProgram default_module [] [] [
    default_main,
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Const_Assign_Static (Identifier "x")
                                 Nothing (Leaf (Lit (LitInt 42)))
    ] Nothing)]

subber_postfix_oper :: CheckedProgram
subber_postfix_oper = CheckedProgram default_module [] [] [
    default_main,
    SubDefn (Identifier "subby_postfix_oper") Nothing Nothing (Stmts [
        Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
        Stmt_Postfix_Oper (Identifier "x") "++"
        ] Nothing)]

subber_while :: CheckedProgram
subber_while = CheckedProgram default_module [] [] [
    default_main,
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_While (Leaf (Name "false")) (Stmts [
            Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
            Stmt_Postfix_Oper (Identifier "x") "++"] Nothing)
        ] Nothing)]

subber_if :: CheckedProgram
subber_if = CheckedProgram default_module [] [] [
    default_main,
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_If (Leaf (Name "false")) (Stmts [
            Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
            Stmt_Postfix_Oper (Identifier "x") "++"
            ] Nothing) Nothing
        ] Nothing)]

subber_if_else :: CheckedProgram
subber_if_else = CheckedProgram default_module [] [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
        Stmt_If (Leaf (Lit (LitInt 0))) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"
            ] Nothing)
            (Just (Stmts [
                (Stmt_Postfix_Oper (Identifier "x") "--")] Nothing))
        ] Nothing),
    MainDefn (default_main_param False) Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing
        ] Nothing)]

subber_unless :: CheckedProgram
subber_unless = CheckedProgram default_module [] [] [
    default_main,
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Unless (Leaf (Name "false")) (Stmts [
            Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
            Stmt_Postfix_Oper (Identifier "x") "++"
            ] Nothing)
            Nothing
            ] Nothing )]

subber_unless_else :: CheckedProgram
subber_unless_else = CheckedProgram default_module [] [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
        Stmt_Unless (Leaf (Name "false")) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"] Nothing)
            (Just (Stmts [
                (Stmt_Postfix_Oper (Identifier "x") "--")] Nothing))] Nothing),
    MainDefn (default_main_param False) Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing] Nothing)]

sub_while :: CheckedProgram
sub_while = CheckedProgram default_module [] [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
        Stmt_While (Leaf (Name "true")) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"] Nothing)] Nothing),
    MainDefn (default_main_param False) Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing] Nothing)]

sub_until :: CheckedProgram
sub_until = CheckedProgram default_module [] [] [
    SubDefn (Identifier "subby") Nothing Nothing (Stmts [
        Stmt_Var_Declare (Identifier "x") Nothing (Leaf (Lit (LitInt 41))),
        Stmt_Until (Leaf (Name "false")) (Stmts [
            Stmt_Postfix_Oper (Identifier "x") "++"] Nothing)] Nothing),
    MainDefn (default_main_param False) Nothing (Stmts [
        Stmt_Sub_Call (Identifier "subby") Nothing] Nothing)]
