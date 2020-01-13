module Common where
    Stmt(..),
    Param(..),
    Identifier(..),
    Stmts(..),
    Program(..),
    Raw_Expr(..),
    Top_Level_Defn(..),
    Expr(..),
    Endable_Stmts(..),
    ModuleName(..),
    Import(..),
    Imports,

    Parser,
    ParserState,
    empty_state

    ) where

import Text.Parsec (Parsec)

-- import qualified Data.Map.Strict as Map (Map, singleton, member, insert)
import qualified Data.Map.Strict as Map (Map, empty)
import Data.List.NonEmpty ( NonEmpty(..) )

import Parser.ExprParser


{-
-- the depth and the number of spaces at each level
-- type Indentation = (Int, [Int])
-}

type Bindings = Map.Map Identifier ASTree -- FIXME: is ASTree correct here?

type Indentation = Int -- for now, indentation must be exactly 4 spaces

-- FIXME: should this be a list of maps (for levels of scope)?
type ParserState = (Indentation, NonEmpty Bindings)

empty_state :: ParserState
empty_state = (0, Map.empty :| [])

-- type Parser a = Parsec String Indentation a
type Parser a = Parsec String ParserState a

newtype Identifier = Identifier String
                   deriving (Eq, Read, Show, Ord)

-- instance Show Identifier where
--     show (Identifier x) = show x

data Program = Program (Maybe ModuleName) Imports Body
    deriving Show

newtype ModuleName = ModuleName String deriving (Read, Show)
type Imports = [Import]
type Body = [Top_Level_Defn]

newtype Import = Import String deriving (Read, Show)

newtype Stmts = Stmts [Stmt] deriving Show

newtype Param = Param [Identifier] deriving (Show)


data Top_Level_Defn = Top_Level_Const_Defn Identifier ASTree
                    | FuncDefn Identifier Param Stmts
                    | ShortFuncDefn Identifier Param ASTree
                    | SubDefn Identifier (Maybe Param) Stmts
                    | MainDefn (Maybe Param) Stmts
                    deriving Show

data Raw_Expr = Raw_Expr String deriving (Read, Show)

data Expr = Expr_Number Integer
          | Expr_CharLit Char
          | Expr_StringLit String
          | Expr_Identifier Identifier
          | Expr_BoolLit Bool -- FIXME generalize this. bools are just like any other `datatype`
          | Expr_FuncCall String Expr
          | Expr_Prefix_Oper String Expr
          | Expr_Infix_Oper Expr String Expr -- FIXME get rid of this?
          deriving Show

data Stmt = Stmt_While ASTree Stmts
          | Stmt_Until ASTree Stmts
          | Stmt_If ASTree Stmts (Maybe Stmts)
          | Stmt_Unless ASTree Stmts (Maybe Stmts)
          | Stmt_Sub_Call Identifier (Maybe ASTree)
          | Stmt_Postfix_Oper Identifier String
          | Stmt_Const_Assign Identifier ASTree
          | Stmt_Var_Assign Identifier ASTree
          | Stmt_Return (Maybe ASTree)
          deriving (Show)

data Endable_Stmts = Stmt_If_End | Stmt_While_End | Stmt_Unless_End | Stmt_Until_End
