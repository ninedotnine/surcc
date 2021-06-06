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
import Data.Text (Text)

generate :: CheckedProgram -> Text
-- generate (Program name imports body) =
generate (CheckedProgram _ _ body) = runtime <> mconcat (gen <$> body)

class Generatable a where
    gen :: a -> Text

instance Generatable ASTree where
    gen = generate_expr

instance Generatable Identifier where
    gen = generate_identifier

instance Generatable Param where
    gen (Param param _) = "union _souc_obj " <> gen param

instance Generatable Top_Level_Defn where
    gen (Top_Level_Const_Defn name _ expr) =
        "const union _souc_obj " <> gen name <> " = " <> gen expr <> ";\n"
    gen (FuncDefn name param _ stmts) =
        "union _souc_obj " <> gen name <> "(" <> gen param <> ") {" <> body <> "}\n"
            where body = gen stmts
    gen (ShortFuncDefn name param _ expr) =
        "union _souc_obj " <> gen name <> "(" <> gen param  <> ") { return " <>
        gen expr <> "; }\n"
    gen (SubDefn name m_param _ stmts) =
        "void " <> gen name <> "(" <> param <> ") { " <> body <> "}\n" where
            body = gen stmts
            param = case m_param of
                Nothing -> "void"
                Just p -> gen p
    gen (MainDefn m_param _ stmts) =
        "int main(void) { " <> param <> body <> "}\n" where
            body = gen stmts
            param = case m_param of
                Nothing -> ""
                Just p -> gen p <> ";"


instance Generatable Stmts where
    gen (Stmts stmts) = mconcat (gen <$> stmts)

instance Generatable Stmt where
    gen (Stmt_Return m_expr) = "return " <> expr <> "; " where
        expr = case m_expr of
            Nothing -> ""
            Just e -> gen e
    gen (Stmt_Sub_Call name m_expr) = gen name <> "(" <> expr <> "); " where
        expr = case m_expr of
            Nothing -> ""
            Just e -> gen e
    gen (Stmt_Var_Assign name _ expr) =
        "union _souc_obj " <> gen name <> " = " <> gen expr <> "; "
    gen (Stmt_Var_Reassign name expr) = gen name <> " = " <> gen expr <> "; "
    gen (Stmt_Const_Assign name _ expr) =
        "const union _souc_obj " <> gen name <> " = " <> gen expr <> "; "
    gen (Stmt_Postfix_Oper name oper) = gen name <> genplusplus oper <> "; "
    gen (Stmt_While expr stmts) =
        "while ((" <> gen expr <> ")._souc_bool ) { " <> gen stmts <> "} "
    gen (Stmt_Until expr stmts) =
        "while (!((" <> gen expr <> ")._souc_bool )) { " <> gen stmts <> "} "
    gen (Stmt_If expr stmts m_else_stmts) =
        if_branch <> else_branch m_else_stmts where
            if_branch =
                "if ((" <> gen expr <> ")._souc_bool ) { " <> gen stmts <> "} "
            else_branch Nothing = ""
            else_branch (Just else_stmts) =
                "else {" <> gen else_stmts <> "} "
    gen (Stmt_Unless expr stmts m_else_stmts) =
        if_branch <> else_branch m_else_stmts where
            if_branch =
                "if (!((" <> gen expr <> ")._souc_bool )) { " <> gen stmts <> "} "
            else_branch Nothing = ""
            else_branch (Just else_stmts) =
                "else {" <> gen else_stmts <> "} "

genplusplus :: String -> Text
-- FIXME support other operators than "++"
-- also the opers should probably have their own type, not be String
genplusplus _ = "._souc_int++"
