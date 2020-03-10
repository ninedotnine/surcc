module Common (
    Stmt(..),
    Param(..),
    Identifier(..),
    Stmts(..),
    CheckedProgram(..),
    Program(..),
    Top_Level_Defn(..),
--     Endable_Stmts(..),
    ModuleName(..),
    Import(..),
    Imports,

    SouCParser,
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

-- type SouCParser a = Parsec String Indentation a
type SouCParser a = Parsec String ParserState a

newtype Identifier = Identifier String
                   deriving (Eq, Read, Show, Ord)

-- instance Show Identifier where
--     show (Identifier x) = show x

data Program = Program (Maybe ModuleName) Imports Body
    deriving Show

data CheckedProgram = CheckedProgram (Maybe ModuleName) Imports Body
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
