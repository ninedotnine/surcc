{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module SurCC.Common (
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
    Term(..),
    ExprTree(..),
    Operator(..),
    PrefixOperator(..),
    Literal(..),
    Pattern(..),
    Guard(..),
    Refutable(..),
    Bound(..),
    TypeError(..),
    Stmts(..), -- FIXME rename to StmtBlock ?
    Return(..),
    CheckedProgram(..),
    ParseTree(..),
    TypeDef(..),
    TopLevelDefn(..),
    ExportDecl(..),
    SurCModule(..),
    ImportDecl(..),
    Imports,
    Mutability(..),
    Optional(..),
    or_left,
) where

import SurCC.Common.SoucTypes

import Control.Monad (join)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import TextShow (TextShow(..))
import TextShow qualified

data Mutability = Mut | Immut deriving (Eq, Show)

newtype Identifier = Identifier Text
                   deriving (Eq, Ord, IsString, Semigroup)

data ParseTree = ParseTree SurCModule Imports [TypeDef] Body

data CheckedProgram = CheckedProgram SurCModule Imports [Bound] Body

data ExportDecl = ExportDecl Bound

data SurCModule = SurCModule Identifier [ExportDecl]

data ImportDecl = LibImport Text | RelImport Text

type Imports = [ImportDecl]
type Body = [TopLevelDefn]

data Stmts = Stmts [Stmt] (Maybe Return) deriving (Eq, Show)

data TypeDef = EmptyType SoucType
             | UnitType SoucType Identifier
             | SynonymType SoucType SoucType
             | WrapperType SoucType Identifier SoucType
                        -- fixme: a wrapper (a function) is not a term.
             | EnumType SoucType [Identifier]
             | StructType SoucType [Bound]
             | GADType SoucType -- fixme
             deriving (Show)

data Bound = Bound Identifier SoucType deriving Eq

instance Show Bound where
    show (Bound (Identifier i) t) =
        show i <> ": " <> show t


data TypeError = TypeMismatch SoucType SoucType
               | MultipleDeclarations Identifier
               | MultipleTypeDeclarations SoucType
               | Undeclared Identifier
               | BadReassign Identifier
               | MutateImmutable Identifier SoucType -- FIXME replaces BadReassign
               | UnknownData Identifier -- FIXME delete? same as Undeclared
               | UnknownType SoucType
               | ExportedButNotDefined Bound
               | ExportedLocal Identifier
    deriving (Eq)

data Param = Param Identifier (Maybe SoucType) deriving (Eq, Show)

newtype MainParamStdIn = MainParamStdIn Bool deriving (Eq,Show)
newtype MainParamStdOut = MainParamStdOut Bool deriving (Eq,Show)
newtype MainParamStdErr = MainParamStdErr Bool deriving (Eq,Show)
newtype MainParamProgName = MainParamProgName Bool deriving (Eq,Show)
newtype MainParamArgs = MainParamArgs Bool deriving (Eq,Show)
newtype MainParamEnv = MainParamEnv Bool deriving (Eq,Show)
data MainParam = MainParam
    MainParamStdIn
    MainParamStdOut
    MainParamStdErr
    MainParamProgName
    MainParamArgs
    MainParamEnv
    deriving (Eq,Show)

data TopLevelDefn = TopLevelConstDefn Identifier (Maybe SoucType) ExprTree
                    | FuncDefn Identifier Param (Maybe SoucType) Stmts
                    | ShortFuncDefn Identifier Param (Maybe SoucType) ExprTree
                    | SubDefn Identifier (Maybe Param) (Maybe SoucType) Stmts
                    -- FIXME make this take a [Stmt] instead of Stmts?
                    -- main routine should not be Return ed from
                    | MainDefn MainParam (Maybe SoucType) Stmts
                    deriving (Eq, Show)

data Stmt = Stmt_While ExprTree Stmts
          | Stmt_Until ExprTree Stmts
          | Stmt_If ExprTree Stmts (Maybe Stmts)
          | Stmt_Unless ExprTree Stmts (Maybe Stmts)
          | Stmt_Sub_Call Identifier (Maybe ExprTree)
          | Stmt_Postfix_Oper Identifier String
          | Stmt_Const_Assign_Static Identifier (Maybe SoucType) ExprTree
          | Stmt_Const_Assign_Dynamic Identifier (Maybe SoucType) ExprTree
          | Stmt_Var_Declare Identifier (Maybe SoucType) ExprTree
          | Stmt_Var_Reassign Identifier (Maybe SoucType) ExprTree
          deriving (Eq, Show)

newtype Return = Return (Maybe ExprTree)
    deriving (Eq, Show)

data ExprTree = Branch Operator ExprTree ExprTree
            | Twig PrefixOperator ExprTree
            | Signed ExprTree SoucType
            | Leaf Term
            | Match ExprTree [(Pattern, Maybe Guard, ExprTree)]
         deriving (Eq, Show)


data Term = Lit Literal
          | Name Identifier
    deriving (Eq, Show)

data Operator = Plus
              | Minus
              | Splat
              | FieldDiv
              | FloorDiv
              | Modulo -- FIXME delete this
              | Hihat -- FIXME delete this?
              | Equals
              | NotEquals
              | RegexMatch -- FIXME delete this?
              | GreaterThan
              | LesserThan
              | And
              | Or
              | Xor
              | In
              | Comma
              | Iff -- FIXME delete this?
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
            deriving (Eq,Show)

data PrefixOperator = Deref
                    | GetAddr
                    | Negate
                    | ToString
                    | Pure
                    | Join
--                     | Void
--                     | Curry
--                     | Uncurry
                    deriving (Eq, Show)


data Literal = LitInt Integer
             | LitChar Char
             | LitString Text
    deriving (Eq, Show)


data Pattern = PatLit Literal
             | PatConst Identifier
             | PatBinding Identifier
--              | PatNested Identifier Pattern
--              | PatConstructor Constant [Pattern]
             deriving (Eq, Show)


-- whether a pattern can be used in a match that might fail
newtype Refutable = Refutable Bool deriving (Eq, Ord, Show)


newtype Guard = Guard ExprTree
    deriving (Eq, Show)

instance Show Identifier where
    show (Identifier i) = Text.unpack i

class (Foldable f) => Optional f where
    (//) :: f a -> a -> a
    (//) = flip (foldr const)

instance Optional Maybe
instance Optional (Either e)
instance Optional []


or_left :: Maybe a -> b -> Either b a
or_left m_a dfault = case m_a of
    Just x -> Right x
    Nothing -> Left dfault
