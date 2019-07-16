module SouC_Types where

import Text.Parsec (Parsec)

-- import qualified Data.Map.Strict as Map (Map, singleton, member, insert)
import qualified Data.Map.Strict as Map (Map)

{-
-- the depth and the number of spaces at each level
-- type Indentation = (Int, [Int])
-}
-- for now, indentation must be exactly 4 spaces
type Indentation = Int

type Parser a = Parsec String Indentation a

newtype Identifier = Identifier String
--                    deriving (Show)

instance Show Identifier where
    show (Identifier x) = show x

data Program = Program (Maybe ModuleName) Imports Body

newtype ModuleName = ModuleName String deriving Show
type Imports = [Import]
type Body = [Top_Level_Defn]

newtype Import = Import String deriving Show

type Stmts = [Stmt]

type Param = [Identifier]


data Top_Level_Defn = Top_Level_Const_Defn Identifier Expr
                    | FuncDefn Identifier Param [Stmt]
                    | SubDefn Identifier (Maybe Param) [Stmt]
                    deriving (Show)

-- FIXME right now the map maps strings to nothing
type MyState = (Indentation, Map.Map String ())

data Expr = Expr_Number Integer
          | Expr_CharLit Char
          | Expr_StringLit String
          | Expr_Identifier Identifier
          | Expr_BoolLit Bool -- FIXME generalize this. bools are just like any other `datatype`
          | Expr_FuncCall String Expr
          | Expr_Prefix_Oper String Expr
          | Expr_Infix_Oper Expr String Expr -- FIXME get rid of this?
          deriving (Show)

data Stmt = Stmt_While Expr Stmts
          | Stmt_If Expr Stmts (Maybe Stmts)
          | Stmt_Sub_Call Identifier (Maybe Expr)
          | Stmt_Postfix_Oper Identifier String
          | Stmt_Const_Assign Identifier Expr
          | Stmt_Var_Assign Identifier Expr
          deriving (Show)

data Endable_Stmts = Stmt_If_End | Stmt_While_End
