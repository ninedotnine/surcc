{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module SurCC.CodeGen.CodeGen (
    generate
) where

import SurCC.CodeGen.Common
import SurCC.CodeGen.ExprGen (
    gen_expr,
    gen_identifier,
    )
import SurCC.CodeGen.Runtime (runtime)
import SurCC.Common (
    Stmt(..),
    Param(..),
    MainParam(..),
    MainParamStdIn(..),
    MainParamStdOut(..),
    MainParamStdErr(..),
    MainParamProgName(..),
    MainParamArgs(..),
    MainParamEnv(..),
    Identifier(..),
    Stmts(..),
    Return(..),
    CheckedProgram(..),
    TopLevelDefn(..)
    )
import SurCC.Parser.ExprParser (ExprTree)

import Control.Monad.State (evalState)
import Control.Monad.Writer (execWriterT, tell)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text qualified as Text

generate :: CheckedProgram -> Text
generate (CheckedProgram _ _ body) = runtime <> text
    where
        text = evalState (execWriterT (gen body)) 0

class Genny a b | a -> b where
    gen :: a -> Generator b

instance Genny Identifier Text where
    gen = pure . gen_identifier

instance Genny ExprTree Text where
    gen = gen_expr

instance Genny (Maybe ExprTree) Text where
    gen = maybe (pure mempty) gen

instance Genny [TopLevelDefn] () where
    gen tree = traverse_ gen tree

instance Genny TopLevelDefn () where
    gen = \case
        TopLevelConstDefn name _ expr -> do
            n <- gen name
            e <- gen expr
            tell $ "const union _souc_obj " <> n <> " = " <> e <> ";\n"

        FuncDefn name param _ stmts -> do
            n <- gen name
            p <- gen param
            tell $ "union _souc_obj " <> n <> "(" <> p <> ") {\n"
            s <- gen stmts
            tell $ s <> "}\n"

        ShortFuncDefn name param _ expr -> do
            n <- gen name
            p <- gen param
            tell $ "union _souc_obj " <> n <> "(" <> p <> ") {\n"
            -- fixme decls need to be statically allocd
            e <- gen expr
            tell $ "return " <> e <> ";\n}\n"

        SubDefn name m_param _ stmts -> do
            n <- gen name
            p <- gen m_param
            tell $ "void " <> n <> "(" <> p <> ") {\n"
            s <- gen stmts
            tell $ s <> "}\n"

        MainDefn param _ stmts -> do
            tell $ "int main(void) {\n"
            gen param
            s <- gen stmts
            tell $ s <> "}\n"

instance Genny MainParam () where
    gen (MainParam
            (MainParamStdIn stdin)
            (MainParamStdOut stdout)
            (MainParamStdErr stderr)
            (MainParamProgName progname)
            (MainParamArgs args)
            (MainParamEnv env)
        ) = do
        tell $ include stdin "stdin"
            <> include stdout "stdout"
            <> include stderr "stderr"
            <> include progname "program_name"
            <> include args "args"
            <> include env "env"

        where include :: Bool -> Text -> Text
              include b name = if b
                     -- FIXME
                     -- zero is fine for an OutputStream,
                     -- which only exists in SurC (not in C)
                     -- but program_name, args, and env
                     -- will need to have actual values here.
                then "const union _souc_obj _souc_user_"
                     <> name
                     <> " = {._souc_int = 0};\n"
                else ""


instance Genny Param Text where
    gen (Param (Identifier param) _) = do
        pure $ "const union _souc_obj _souc_user_" <> param

instance Genny (Maybe Param) Text where
    gen = maybe (pure "void") gen

instance Genny Stmts Text where
    gen (Stmts stmts m_ret) = do
        body <- mconcat <$> traverse gen stmts
        case m_ret of
            Nothing -> pure body
            (Just (Return m_expr)) -> do
                expr <- gen m_expr
                pure $ body <> "return " <> expr <> ";\n"

instance Genny (Maybe Stmts) Text where
    gen = maybe (pure "") gen

instance Genny Stmt Text where
    gen = \case
        Stmt_Sub_Call name m_expr -> do
            gname <- gen name
            expr <- gen m_expr
            pure $ gname <> "(" <> expr <> ");\n"
        Stmt_Var_Declare name _ expr -> do
            gname <- gen name
            gexpr <- gen expr
            pure $ "union _souc_obj " <> gname <> " = " <> gexpr <> ";\n"
        Stmt_Var_Reassign name _ expr -> do
            gname <- gen name
            gexpr <- gen expr
            pure $ gname <> " = " <> gexpr <> ";\n"
        Stmt_Const_Assign_Static name _ expr -> do
            gname <- gen name
            gexpr <- gen expr
            pure $ "const union _souc_obj " <> gname <> " = " <>
                    gexpr <> ";\n"
        Stmt_Const_Assign_Dynamic name _ expr -> do
            gname <- gen name
            gexpr <- gen expr
            pure $ "const union _souc_obj " <> gname <> " = " <>
                    gexpr <> ";\n"
        Stmt_Postfix_Oper name op -> do
            gname <- gen name
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
