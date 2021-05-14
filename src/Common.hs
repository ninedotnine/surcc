{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Common (
    Stmt(..),
    Param(..),
    Identifier(..),
    Term(..),
    ASTree(..),
    Operator(..),
    PrefixOperator(..),
    SoucType(..),
    pattern SoucFn,
    pattern SoucMaybe,
    Bound(..),
    TypeError(..),
    Stmts(..),
    CheckedProgram(..),
    Program(..),
    Top_Level_Defn(..),
--     Endable_Stmts(..),
    ExportDecl(..),
    SoucModule(..),
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
import Data.Hashable (Hashable)


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
                   deriving (Eq, Read, Show, Ord, IsString, Semigroup, Hashable)

-- instance Show Identifier where
--     show (Identifier x) = show x

data Program = Program (Maybe SoucModule) Imports Body
    deriving Show

data CheckedProgram = CheckedProgram (Maybe SoucModule) Imports Body
    deriving Show

data ExportDecl = ExportDecl Identifier SoucType deriving (Show)
data SoucModule = SoucModule String [ExportDecl] deriving (Show)

type Imports = [Import]
type Body = [Top_Level_Defn]

newtype Import = Import String deriving (Read, Show)

newtype Stmts = Stmts [Stmt] deriving (Show, Eq)

data SoucType = SoucType String
              | SoucRoutn (Maybe SoucType) -- param only, because return must be "IO"
              | SoucPair SoucType SoucType
              | SoucEither SoucType SoucType
              | SoucList SoucType
              | SoucTypeConstructor String [SoucType]
              deriving (Eq)

pattern SoucFn :: SoucType -> SoucType -> SoucType
pattern SoucFn t0 t1 = SoucTypeConstructor "Fn" [t0, t1]

pattern SoucMaybe :: SoucType -> SoucType
pattern SoucMaybe t = SoucTypeConstructor "Maybe" [t]

data Bound = Bound Identifier SoucType deriving Eq

instance Show Bound where
    show (Bound (Identifier i) t) = "Bound " ++ i ++ ": " ++ show t

data TypeError = TypeMismatch SoucType SoucType
               | MultipleDeclarations Identifier
               | Undeclared Identifier
               | UnknownData String
               | ExportedButNotDefined Bound
    deriving (Eq)

instance Show TypeError where
    show (TypeMismatch t0 t1) = "mismatch: expected " <> show t0 <> " but got " <> show t1
    show (MultipleDeclarations name) = "multiple declarations of " <> show name
    show (Undeclared name) = "undeclared " <> show name
    show (UnknownData name) = "unknown data constructor: " <> show name
    show (ExportedButNotDefined name) = "declared " <> show name <> " was not defined"

data Param = Param Identifier (Maybe SoucType) deriving (Show, Eq)


data Top_Level_Defn = Top_Level_Const_Defn Identifier (Maybe SoucType) ASTree
                    | FuncDefn Identifier Param (Maybe SoucType) Stmts
                    | ShortFuncDefn Identifier Param (Maybe SoucType) ASTree
                    | SubDefn Identifier (Maybe Param) (Maybe SoucType) Stmts
                    | MainDefn (Maybe Param) (Maybe SoucType) Stmts
                    deriving (Show, Eq)

data Stmt = Stmt_While ASTree Stmts
          | Stmt_Until ASTree Stmts
          | Stmt_If ASTree Stmts (Maybe Stmts)
          | Stmt_Unless ASTree Stmts (Maybe Stmts)
          | Stmt_Sub_Call Identifier (Maybe ASTree)
          | Stmt_Postfix_Oper Identifier String
          | Stmt_Const_Assign Identifier (Maybe SoucType) ASTree
          | Stmt_Var_Assign Identifier (Maybe SoucType) ASTree
          | Stmt_Return (Maybe ASTree)
          deriving (Show, Eq)

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Signed ASTree SoucType
            | Leaf Term
         deriving (Show, Eq)

data Term = LitInt Integer
          | LitChar Char
          | LitBool Bool
          | LitString String
          | Var Identifier
          | Constructor String
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

instance Show SoucType where
    show (SoucType t) = show t
    show (SoucRoutn t) = show t <> " -> IO"
    show (SoucPair t0 t1) = show t0 <> " & " <> show t1
    show (SoucEither t0 t1) = show t0 <> " | " <> show t1
    show (SoucList t) = '[': show t <> "]"
    show (SoucTypeConstructor t ts) = t <> "(" <> show ts <> ")"
