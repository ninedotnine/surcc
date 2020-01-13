module CodeGen.CodeGen (generate) where

import CodeGen.ExprGen (generate_expr)
import SouC_Types
import ExprParser

generate :: Program -> String
-- generate (Program name imports body) =
generate (Program _ _ body) = includes ++ concat (map gen body)
    where includes =
            "#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>\n"

class Generatable a where
    gen :: a -> String

instance Generatable ASTree where
    gen = generate_expr

instance Generatable Identifier where
    gen (Identifier v) = v

instance Generatable Raw_Expr where
    gen (Raw_Expr v) = v

instance Generatable Param where
    gen (Param []) = ""
    gen (Param [param]) = "int " ++ gen param -- FIXME
    gen _ = "FIXME LOL"

instance Generatable Top_Level_Defn where
    gen (Top_Level_Const_Defn name raw_expr) =
        "const int " ++ gen name ++ " = " ++ gen raw_expr ++ ";\n"
    gen (FuncDefn name param stmts) =
        "int " ++ gen name ++ "(" ++ gen param ++ ") {" ++ body ++ "}\n"
            where body = gen stmts
    gen (ShortFuncDefn name param raw_expr) =
        "int " ++ gen name ++ "(" ++ gen param  ++ ") { return " ++
        gen raw_expr ++ "; }\n"
    gen (SubDefn name m_param stmts) =
        "void " ++ gen name ++ "(" ++ param ++ ") { " ++ body ++ "}\n" where
            body = gen stmts
            param = case m_param of
                Nothing -> "void"
                Just p -> gen p
    gen (MainDefn m_param stmts) =
        "int main (" ++ param ++ ") { " ++ body ++ "}\n" where
            body = gen stmts
            param = case m_param of
                Nothing -> "void"
                Just p -> gen p

instance Generatable Stmts where
    gen (Stmts stmts) = concat $ map gen stmts

instance Generatable Stmt where
    gen (Stmt_Return m_raw_expr) = "return " ++ raw_expr ++ "; " where
        raw_expr = case m_raw_expr of
            Nothing -> ""
            Just e -> gen e
    gen (Stmt_Sub_Call name m_raw_expr) =
        gen name ++ "(" ++ raw_expr ++ "); " where
            raw_expr = case m_raw_expr of
                Nothing -> ""
                Just e -> gen e
    gen (Stmt_Var_Assign name raw_expr) =
        "int " ++ gen name ++ " = " ++ gen raw_expr ++ "; "
    gen (Stmt_Const_Assign name raw_expr) =
        "const int " ++ gen name ++ " = " ++ gen raw_expr ++ "; "
    gen (Stmt_Postfix_Oper name oper) = gen name ++ oper ++ "; "
    gen (Stmt_While raw_expr stmts) =
        "while ( " ++ gen raw_expr ++ " ) { " ++ gen stmts ++ "} "
    gen (Stmt_Until raw_expr stmts) =
        "while (!( " ++ gen raw_expr ++ " )) { " ++ gen stmts ++ "} "
    gen (Stmt_If raw_expr stmts m_else_stmts) =
        if_branch ++ else_branch m_else_stmts where
            if_branch =
                "if ( " ++ gen raw_expr ++ " ) { " ++ gen stmts ++ "} "
            else_branch Nothing = ""
            else_branch (Just else_stmts) =
                "else {" ++ gen else_stmts ++ "} "
    gen (Stmt_Unless raw_expr stmts m_else_stmts) =
        if_branch ++ else_branch m_else_stmts where
            if_branch =
                "if (!( " ++ gen raw_expr ++ " )) { " ++ gen stmts ++ "} "
            else_branch Nothing = ""
            else_branch (Just else_stmts) =
                "else {" ++ gen else_stmts ++ "} "
