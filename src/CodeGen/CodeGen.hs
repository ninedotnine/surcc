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
    TopLevelDefn(..)
    )
import Parser.ExprParser (ExprTree)

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

instance Genny ExprTree (Text,Text) where
    gen = generate_expr

instance Genny (Maybe ExprTree) (Text,Text) where
    gen = maybe (pure ("","")) gen

instance Genny [TopLevelDefn] () where
    gen tree = traverse_ gen tree

instance Genny TopLevelDefn () where
    gen = \case
        TopLevelConstDefn name _ expr -> do
            (CIdentifier n) <- gen name
            (decls, e) <- gen expr
            tell $ decls <> "const union _souc_obj " <> n <> " = " <> e <> ";\n"

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
            (decls, e) <- gen expr -- fixme decls need to be statically allocd
            tell $ decls <> "return " <> e <> ";\n}\n"

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
            (decls, expr) <- gen m_expr
            pure $ decls <> "return " <> expr <> ";\n"
        Stmt_Sub_Call name m_expr -> do
            (CIdentifier gname) <- gen name
            (decls, expr) <- gen m_expr
            pure $ decls <> gname <> "(" <> expr <> ");\n"
        Stmt_Var_Assign name _ expr -> do
            (CIdentifier gname) <- gen name
            (decls, gexpr) <- gen expr
            pure $ decls <> "union _souc_obj " <> gname <> " = " <> gexpr <> ";\n"
        Stmt_Var_Reassign name expr -> do
            (CIdentifier gname) <- gen name
            (decls, gexpr) <- gen expr
            pure $ decls <> gname <> " = " <> gexpr <> ";\n"
        Stmt_Const_Assign name _ expr -> do
            (CIdentifier gname) <- gen name
            (decls, gexpr) <- gen expr
            pure $ decls <> "const union _souc_obj " <> gname <> " = " <>
                    gexpr <> ";\n"
        Stmt_Postfix_Oper name op -> do
            (CIdentifier gname) <- gen name
            pure $ "\n; // fixme gname: " <> gname <>
                        " unsupported operator: `" <> Text.pack op <> "`\n"
        Stmt_While expr stmts -> do
            (decls, gexpr) <- gen expr
            gstmts <- gen stmts
            pure $ decls <> "while ((" <> gexpr <> ")._souc_bool ) {\n" <>
                    gstmts <> "\n}\n"
        Stmt_Until expr stmts -> do
            (decls, gexpr) <- gen expr
            gstmts <- gen stmts
            pure $ decls <> "while (!((" <> gexpr <> ")._souc_bool )) {\n" <>
                    gstmts <> "\n}\n"
        Stmt_If expr stmts m_else_stmts -> do
            (decls, gexpr) <- gen expr
            gstmts <- gen stmts
            gelse_stmts <- gen m_else_stmts
            pure $ decls <> "if ((" <> gexpr <> ")._souc_bool ) {\n" <>
                    gstmts <> "} else {" <> gelse_stmts <> "\n}\n"
        Stmt_Unless expr stmts m_else_stmts -> do
            (decls, gexpr) <- gen expr
            gstmts <- gen stmts
            gelse_stmts <- gen m_else_stmts
            pure $ decls <> "if (!((" <> gexpr <> ")._souc_bool )) {\n" <>
                    gstmts <> "} else {" <> gelse_stmts <> "\n}\n"
