{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Common (
    Stmt(..),
    Param(..),
    Identifier(..),
    Term(..),
    ASTree(..),
    Operator(..),
    PrefixOperator(..),
    TypeName(..),
    TypeError(..),
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
import Data.String (IsString)


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
                   deriving (Eq, Read, Show, Ord, IsString, Semigroup)

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

newtype Stmts = Stmts [Stmt] deriving (Show, Eq)

newtype TypeName = TypeName String deriving (Show, Eq, IsString, Semigroup)

data TypeError = TypeMismatch TypeName TypeName
               | MultipleDeclarations Identifier
               | Undeclared Identifier
    deriving (Show, Eq)

data Param = Param Identifier (Maybe TypeName) deriving (Show, Eq)


data Top_Level_Defn = Top_Level_Const_Defn Identifier (Maybe TypeName) ASTree
                    | FuncDefn Identifier Param (Maybe TypeName) Stmts
                    | ShortFuncDefn Identifier Param (Maybe TypeName) ASTree
                    | SubDefn Identifier (Maybe Param) (Maybe TypeName) Stmts
                    | MainDefn (Maybe Param) (Maybe TypeName) Stmts
                    deriving (Show, Eq)

data Stmt = Stmt_While ASTree Stmts
          | Stmt_Until ASTree Stmts
          | Stmt_If ASTree Stmts (Maybe Stmts)
          | Stmt_Unless ASTree Stmts (Maybe Stmts)
          | Stmt_Sub_Call Identifier (Maybe ASTree)
          | Stmt_Postfix_Oper Identifier String
          | Stmt_Const_Assign Identifier ASTree
          | Stmt_Var_Assign Identifier ASTree
          | Stmt_Return (Maybe ASTree)
          deriving (Show, Eq)

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Signed ASTree TypeName
            | Leaf Term
         deriving (Show, Eq)

data Term = LitInt Integer
          | LitChar Char
          | LitBool Bool
          | LitString String
          | Var Identifier
    deriving (Eq, Show)

data Operator = Plus
              | Minus
              | Splat
              | Divide
              | FloorDiv
              | Modulo
              | Hihat
              | Equals
              | NotEquals
              | RegexMatch
              | GreaterThan
              | LesserThan
              | And
              | Or
              | Xor
              | In
              | Tuple
              | Iff
              | FromMaybe
              | Prepend
              | Append
              | Combine
              | Index
              | Lookup
              | Apply
              | FlipApply
              | Map
              | FlipMap
              | Applicative
              | FlipApplicative
              | SequenceRight
              | SequenceLeft
              | BindRight
              | BindLeft
            deriving Eq

data PrefixOperator = Deref
                    | GetAddr
                    | Negate
                    | ToString
                    | Pure
                    | Join
                    deriving Eq

instance Show Operator where
    show Plus           = "+"
    show Minus          = "-"
    show Splat          = "*"
    show Divide         = "/"
    show FloorDiv       = "//"
    show Modulo         = "%"
    show Hihat          = "^"
    show Equals         = "=="
    show Combine        = "<>"
    show NotEquals      = "=/="
    show RegexMatch     = "=~"
    show GreaterThan    = ">"
    show LesserThan     = "<"
    show And            = "&&" -- FIXME
    show Or             = "||" -- FIXME
    show Xor            = "><"
    show In             = ">|#|<"
    show Tuple          = ","
    show Iff            = "?"
    show FromMaybe      = "??"
    show Prepend        = ">>"
    show Append         = "<<"
    show Index          = "#"
    show Lookup         = "##"
    show Apply          = "~&"
    show FlipApply      = "&"
    show Map            = "<~&>"
    show FlipMap        = "<&>"
    show Applicative    = "<~*>"
    show FlipApplicative = "<*>"
    show SequenceRight  = "*>"
    show SequenceLeft   = "<*"
    show BindRight      = ">>="
    show BindLeft       = "=<<"

instance Show PrefixOperator where
    show Deref    = "!"
    show GetAddr  = "@"
    show Negate   = "~"
    show ToString = "$"
    show Pure     = "^*^"
    show Join     = ">*<"
