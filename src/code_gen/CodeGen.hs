module CodeGen (generate) where

import SouC_Types

class Generatable a where
    gen :: a -> String

instance Generatable Identifier where
    gen (Identifier v) = v

instance Generatable Raw_Expr where
    gen (Raw_Expr v) = v


generate :: Program -> String
-- generate (Program name imports body) =
generate (Program _ _ body) = includes ++ concat (map generate_top_level body)
    where includes =
            "#include <stdio.h>\n#include <stdbool.h>\n#include <stdlib.h>\n"

generate_param :: Param -> String
generate_param [] = ""
generate_param [param] = "int " ++ gen param -- FIXME
generate_param _ = "FIXME LOL"

generate_top_level :: Top_Level_Defn -> String
generate_top_level (Top_Level_Const_Defn name raw_expr) =
    "const int " ++ gen name ++ " = " ++ gen raw_expr ++ ";\n"
generate_top_level (FuncDefn name param stmts) =
    "int " ++ gen name ++ "(" ++ generate_param param ++ ") {" ++ body ++ "}\n"
        where body = generate_stmts stmts
generate_top_level (ShortFuncDefn name param raw_expr) =
    "int " ++ gen name ++ "(" ++ generate_param param  ++ ") { return " ++
    gen raw_expr ++ "; }\n"
generate_top_level (SubDefn name m_param stmts) =
    "void " ++ gen name ++ "(" ++ param ++ ") { " ++ body ++ "}\n" where
        body = generate_stmts stmts
        param = case m_param of
            Nothing -> "void"
            Just p -> generate_param p
generate_top_level (MainDefn m_param stmts) =
    "int main (" ++ param ++ ") { " ++ body ++ "}\n" where
        body = generate_stmts stmts
        param = case m_param of
            Nothing -> "void"
            Just p -> generate_param p

generate_stmts :: Stmts -> String
generate_stmts (Stmts stmts) = concat $ map generate_stmt stmts

generate_stmt :: Stmt -> String
generate_stmt (Stmt_Return m_raw_expr) = "return " ++ raw_expr ++ "; " where
    raw_expr = case m_raw_expr of
        Nothing -> ""
        Just e -> gen e
generate_stmt (Stmt_Sub_Call name m_raw_expr) =
    gen name ++ "(" ++ raw_expr ++ "); " where
        raw_expr = case m_raw_expr of
            Nothing -> ""
            Just e -> gen e
generate_stmt (Stmt_Var_Assign name raw_expr) =
    "int " ++ gen name ++ " = " ++ gen raw_expr ++ "; "
generate_stmt (Stmt_Const_Assign name raw_expr) =
    "const int " ++ gen name ++ " = " ++ gen raw_expr ++ "; "
generate_stmt (Stmt_Postfix_Oper name oper) = gen name ++ oper ++ "; "
generate_stmt (Stmt_While raw_expr stmts) =
    "while ( " ++ gen raw_expr ++ " ) { " ++ generate_stmts stmts ++ "} "
generate_stmt (Stmt_Until raw_expr stmts) =
    "while (!( " ++ gen raw_expr ++ " )) { " ++ generate_stmts stmts ++ "} "
generate_stmt (Stmt_If raw_expr stmts m_else_stmts) =
    if_branch ++ else_branch m_else_stmts where
        if_branch =
            "if ( " ++ gen raw_expr ++ " ) { " ++ generate_stmts stmts ++ "} "
        else_branch Nothing = ""
        else_branch (Just else_stmts) =
            "else {" ++ generate_stmts else_stmts ++ "} "
generate_stmt (Stmt_Unless raw_expr stmts m_else_stmts) =
    if_branch ++ else_branch m_else_stmts where
        if_branch =
            "if (!( " ++ gen raw_expr ++ " )) { " ++ generate_stmts stmts ++ "} "
        else_branch Nothing = ""
        else_branch (Just else_stmts) =
            "else {" ++ generate_stmts else_stmts ++ "} "
