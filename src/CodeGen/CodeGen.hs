module CodeGen.CodeGen (generate) where

import CodeGen.ExprGen (generate_expr, generate_identifier)
import Builtins
import CodeGen.Runtime (runtime)
import Common (
    Stmt(..),
    Param(..),
    Identifier(..),
    Stmts(..),
    CheckedProgram(..),
    Top_Level_Defn(..)
    )
import Parser.ExprParser

import Data.Maybe (fromMaybe)

generate :: CheckedProgram -> String
-- generate (Program name imports body) =
generate (CheckedProgram _ _ body) = runtime ++ concat (map gen body)

class Generatable a where
    gen :: a -> String

instance Generatable ASTree where
    gen = generate_expr

instance Generatable Identifier where
    gen = generate_identifier

instance Generatable Param where
    gen (Param param _) = "int " ++ gen param -- FIXME

instance Generatable Top_Level_Defn where
    gen (Top_Level_Const_Defn name _ expr) =
        "const int " ++ gen name ++ " = " ++ gen expr ++ ";\n"
    gen (FuncDefn name param _ stmts) =
        "int " ++ gen name ++ "(" ++ gen param ++ ") {" ++ body ++ "}\n"
            where body = gen stmts
    gen (ShortFuncDefn name param _ expr) =
        "int " ++ gen name ++ "(" ++ gen param  ++ ") { return " ++
        gen expr ++ "; }\n"
    gen (SubDefn name m_param _ stmts) =
        "void " ++ gen name ++ "(" ++ param ++ ") { " ++ body ++ "}\n" where
            body = gen stmts
            param = case m_param of
                Nothing -> "void"
                Just p -> gen p
    gen (MainDefn m_param _ stmts) =
        "int main (" ++ param ++ ") { " ++ body ++ "}\n" where
            body = gen stmts
            param = case m_param of
                Nothing -> "void"
                Just p -> gen p

instance Generatable Stmts where
    gen (Stmts stmts) = concat $ map gen stmts

instance Generatable Stmt where
    gen (Stmt_Return m_expr) = "return " ++ expr ++ "; " where
        expr = case m_expr of
            Nothing -> ""
            Just e -> gen e
    gen (Stmt_Sub_Call name m_expr) = gen name ++ "(" ++ expr ++ "); " where
        expr = case m_expr of
            Nothing -> ""
            Just e -> gen e
    gen (Stmt_Var_Assign name _ expr) =
        "int " ++ gen name ++ " = " ++ gen expr ++ "; "
    gen (Stmt_Const_Assign name _ expr) =
        "const int " ++ gen name ++ " = " ++ gen expr ++ "; "
    gen (Stmt_Postfix_Oper name oper) = gen name ++ oper ++ "; "
    gen (Stmt_While expr stmts) =
        "while ( " ++ gen expr ++ " ) { " ++ gen stmts ++ "} "
    gen (Stmt_Until expr stmts) =
        "while (!( " ++ gen expr ++ " )) { " ++ gen stmts ++ "} "
    gen (Stmt_If expr stmts m_else_stmts) =
        if_branch ++ else_branch m_else_stmts where
            if_branch =
                "if ( " ++ gen expr ++ " ) { " ++ gen stmts ++ "} "
            else_branch Nothing = ""
            else_branch (Just else_stmts) =
                "else {" ++ gen else_stmts ++ "} "
    gen (Stmt_Unless expr stmts m_else_stmts) =
        if_branch ++ else_branch m_else_stmts where
            if_branch =
                "if (!( " ++ gen expr ++ " )) { " ++ gen stmts ++ "} "
            else_branch Nothing = ""
            else_branch (Just else_stmts) =
                "else {" ++ gen else_stmts ++ "} "
