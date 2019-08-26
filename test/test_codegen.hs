-- this test only ensures that *something* is generated for each program tree.
{-# OPTIONS_GHC -Wall #-}

import CodeGen
import SouC_Types

test :: Program -> IO ()
test prog = do
    print prog
    print $ generate prog

main :: IO ()
main = do
    putStrLn "testing that code is generated for various program trees..."
    test empty_parse_tree
    test conster
    test func
    test func2
    test mainer
    test mainer2
    test subber
    test subber_ass
    test subber_postfix_oper

empty_parse_tree :: Program
empty_parse_tree = Program Nothing [] []

conster :: Program
conster = Program Nothing [] [Top_Level_Const_Defn (Identifier "x") (Raw_Expr "42")]

func :: Program
func = Program Nothing [] [ShortFuncDefn (Identifier "f") [Identifier "x"] (Raw_Expr "42")]

func2 :: Program
func2 = Program Nothing [] [FuncDefn (Identifier "f") [Identifier "x"] [Stmt_Return (Just (Raw_Expr "42"))]]

mainer :: Program
mainer = Program Nothing [] [SubDefn (Identifier "main") Nothing [Stmt_Return Nothing]]

mainer2 :: Program
mainer2 = Program Nothing [] [SubDefn (Identifier "main") Nothing []]

subber :: Program
subber = Program Nothing [] [SubDefn (Identifier "subby") Nothing [Stmt_Sub_Call (Identifier "panic") Nothing]]

subber_ass :: Program
subber_ass = Program Nothing [] [SubDefn (Identifier "subby") Nothing [Stmt_Var_Assign (Identifier "x") (Raw_Expr "42")]]

subber_postfix_oper :: Program
subber_postfix_oper = Program Nothing [] [SubDefn (Identifier "subby") Nothing [Stmt_Var_Assign (Identifier "x") (Raw_Expr "41"), Stmt_Postfix_Oper (Identifier "x") "++"]]
