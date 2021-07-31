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
    SoucKind(..),
    SoucType(..),
    TypeVar(..),
    pattern SoucIO,
    pattern SoucBool,
    pattern SoucInteger,
    pattern SoucChar,
    pattern SoucString,
    pattern SoucFn,
    pattern SoucRoutn,
    pattern SoucMaybe,
    pattern SoucList,
    pattern SoucPair,
    pattern SoucEither,
    Bound(..),
    TypeError(..),
    Stmts(..),
    CheckedProgram(..),
    ParseTree(..),
    Top_Level_Defn(..),
    ExportDecl(..),
    SoucModule(..),
    ImportDecl(..),
    Imports,
    Mutability(..),
    ) where

import Control.Monad (join)
import Data.Hashable (Hashable)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text

data Mutability = Mut | Immut deriving (Show, Eq)

newtype Identifier = Identifier Text
                   deriving (Eq, Read, Show, Ord, IsString, Semigroup, Hashable)

data ParseTree = ParseTree SoucModule Imports Body
    deriving Show

data CheckedProgram = CheckedProgram SoucModule Imports Body
    deriving Show

data ExportDecl = ExportDecl Identifier SoucType deriving (Show)
data SoucModule = SoucModule Text [ExportDecl] deriving (Show)

data ImportDecl = LibImport Text | RelImport Text deriving (Read, Show)

type Imports = [ImportDecl]
type Body = [Top_Level_Defn]

newtype Stmts = Stmts [Stmt] deriving (Show, Eq)

newtype SoucKind = SoucKind Word deriving (Eq, Ord)

-- allowed type names are single chars like 'A'
-- or 'T' followed by an increasing number (T0, T1, ...)
data TypeVar = TypeVar (Either Word Char) SoucKind deriving (Eq, Ord, Show)

instance Show SoucKind where
    show (SoucKind 0) = "*"
    show (SoucKind k) = "*" <> join (replicate (fromIntegral k) " => *")

data SoucType = SoucType Text SoucKind
              | SoucTypeConstructor Text SoucKind [SoucType]
              | SoucTypeVar TypeVar
--               | SoucConstrainedType Constraint SoucType
              deriving (Eq)

-- data Constraint = Instance Text SoucType deriving (Eq)

pattern SoucIO :: SoucType
pattern SoucIO = SoucType "IO" (SoucKind 0)

pattern SoucBool :: SoucType
pattern SoucBool = SoucType "Bool" (SoucKind 0)

pattern SoucInteger :: SoucType
pattern SoucInteger = SoucType "Integer" (SoucKind 0)

pattern SoucChar :: SoucType
pattern SoucChar = SoucType "Char" (SoucKind 0)

pattern SoucString :: SoucType
pattern SoucString = SoucType "String" (SoucKind 0)

pattern SoucFn :: SoucType -> SoucType -> SoucType
pattern SoucFn t0 t1 = SoucTypeConstructor "Fn" (SoucKind 2) [t0,t1]

pattern SoucRoutn :: SoucType -> SoucType
pattern SoucRoutn t = SoucTypeConstructor "Sub" (SoucKind 1) [t]

pattern SoucMaybe :: SoucType -> SoucType
pattern SoucMaybe t = SoucTypeConstructor "Maybe" (SoucKind 1) [t]

pattern SoucList :: SoucType -> SoucType
pattern SoucList t = SoucTypeConstructor "List" (SoucKind 1) [t]

pattern SoucPair :: SoucType -> SoucType-> SoucType
pattern SoucPair t0 t1 = SoucTypeConstructor "Pair" (SoucKind 2) [t0,t1]

pattern SoucEither :: SoucType -> SoucType-> SoucType
pattern SoucEither t0 t1 = SoucTypeConstructor "Either" (SoucKind 2) [t0,t1]

data Bound = Bound Identifier SoucType deriving Eq

instance Show Bound where
    show (Bound (Identifier i) t) = "Bound " <> Text.unpack i <> ": " <> show t

data TypeError = TypeMismatch SoucType SoucType
               | MultipleDeclarations Identifier
               | Undeclared Identifier
               | UnknownData Text
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
          | Stmt_Var_Reassign Identifier ASTree
          | Stmt_Return (Maybe ASTree)
          deriving (Show, Eq)

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Signed ASTree SoucType
            | Leaf Term
         deriving (Show, Eq)

data Term = LitInt Integer
          | LitChar Char
          | LitString Text
          | Var Identifier
          | Constructor Text
    deriving (Eq, Show)

data Operator = Plus
              | Minus
              | Splat
              | FieldDiv
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
    show FieldDiv       = "/"
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
    show SoucIO = "IO"
    show (SoucFn t0 t1) = show t0 <> " -> " <> show t1
    show (SoucRoutn t) = show t <> " -> IO"
    show (SoucMaybe t) = '?': show t
    show (SoucList t) = '[': show t <> "]"
    show (SoucPair t0 t1) = show t0 <> " & " <> show t1
    show (SoucEither t0 t1) = show t0 <> " | " <> show t1
    show (SoucType t _) = show t
    show (SoucTypeVar (TypeVar (Left  v) _)) = 'T' : show v
    show (SoucTypeVar (TypeVar (Right v) _)) = v : ""
    show (SoucTypeConstructor t _ ts) = Text.unpack (t <> "(" <> sho ts <> ")")

sho :: Show a => a -> Text
sho = Text.pack . show
