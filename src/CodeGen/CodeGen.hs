{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module CodeGen.CodeGen (
    generate
) where

import CodeGen.Common
import CodeGen.ExprGen (generate_expr, generate_identifier)
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

import Control.Monad.State (evalState)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (traverse_)
import Data.Text (Text)
import qualified Data.Text as Text

generate :: CheckedProgram -> Text
generate (CheckedProgram _ _ body) = runtime <> text
    where
        text = evalState (execWriterT (gen body)) 0

class Genny a b | a -> b where
    gen :: a -> Generator b

instance Genny Identifier CIdentifier where
    gen = pure . generate_identifier

instance Genny ASTree Text where
    gen = generate_expr

instance Genny (Maybe ASTree) Text where
    gen = maybe (pure "") gen

instance Genny [Top_Level_Defn] () where
    gen tree = traverse_ gen tree

instance Genny Top_Level_Defn () where
    gen = \case
        Top_Level_Const_Defn name _ expr -> do
            (CIdentifier n) <- gen name
            e <- gen expr
            tell $ "const union _souc_obj " <> n <> " = " <> e <> ";\n"

        FuncDefn name param _ stmts -> do
            (CIdentifier n) <- gen name
            p <- gen param
            tell $ "union _souc_obj " <> n <> "(" <> p <> ") {\n"
            s <- gen stmts
            tell $ s <> "}\n"

        ShortFuncDefn name param _ expr -> do
            (CIdentifier n) <- gen name
            p <- gen param
            tell $ "union _souc_obj " <> n <> "(" <> p <> ") {\n"
            e <- gen expr
            tell $ "return " <> e <> ";\n}\n"

        SubDefn name m_param _ stmts -> do
            (CIdentifier n) <- gen name
            p <- gen m_param
            tell $ "void " <> n <> "(" <> p <> ") {\n"
            s <- gen stmts
            tell $ s <> "}\n"

        MainDefn m_param _ stmts -> do
            p <- gen m_param
            tell $ "int main(" <> p <> ") {\n"
            s <- gen stmts
            tell $ s <> "}\n"

instance Genny Param Text where
    gen (Param (Identifier param) _) = do
        pure $ "const union _souc_obj _souc_user_" <> param

instance Genny (Maybe Param) Text where
    gen = maybe (pure "void") gen

instance Genny Stmts Text where
    gen (Stmts stmts) = mconcat <$> traverse gen stmts

instance Genny (Maybe Stmts) Text where
    gen = maybe (pure "") gen

instance Genny Stmt Text where
    gen = \case
        Stmt_Return m_expr -> do
            expr <- gen m_expr
            pure $ "return " <> expr <> ";\n"
        Stmt_Sub_Call name m_expr -> do
            (CIdentifier gname) <- gen name
            expr <- gen m_expr
            pure $ gname <> "(" <> expr <> ");\n"
        Stmt_Var_Assign name _ expr -> do
            (CIdentifier gname) <- gen name
            gexpr <- gen expr
            pure $ "union _souc_obj " <> gname <> " = " <> gexpr <> ";\n"
        Stmt_Var_Reassign name expr -> do
            (CIdentifier gname) <- gen name
            gexpr <- gen expr
            pure $ gname <> " = " <> gexpr <> ";\n"
        Stmt_Const_Assign name _ expr -> do
            (CIdentifier gname) <- gen name
            gexpr <- gen expr
            pure $ "const union _souc_obj " <> gname <> " = " <>
                    gexpr <> ";\n"
        Stmt_Postfix_Oper name op -> do
            (CIdentifier gname) <- gen name
            pure $ "\n; // fixme gname: " <> gname <>
                        " unsupported operator: `" <> Text.pack op <> "`\n"
        Stmt_While expr stmts -> do
            gexpr <- gen expr
            gstmts <- gen stmts
            pure $ "while ((" <> gexpr <> ")._souc_bool ) {\n" <>
                    gstmts <> "\n}\n"
        Stmt_Until expr stmts -> do
            gexpr <- gen expr
            gstmts <- gen stmts
            pure $ "while (!((" <> gexpr <> ")._souc_bool )) {\n" <>
                    gstmts <> "\n}\n"
        Stmt_If expr stmts m_else_stmts -> do
            gexpr <- gen expr
            gstmts <- gen stmts
            gelse_stmts <- gen m_else_stmts
            pure $ "if ((" <> gexpr <> ")._souc_bool ) {\n" <>
                    gstmts <> "} else {" <> gelse_stmts <> "\n}\n"
        Stmt_Unless expr stmts m_else_stmts -> do
            gexpr <- gen expr
            gstmts <- gen stmts
            gelse_stmts <- gen m_else_stmts
            pure $ "if (!((" <> gexpr <> ")._souc_bool )) {\n" <>
                    gstmts <> "} else {" <> gelse_stmts <> "\n}\n"
