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
import TextShow (TextShow(..))
import qualified TextShow

data Mutability = Mut | Immut deriving (Eq)

newtype Identifier = Identifier Text
                   deriving (Eq, Read, Ord, IsString, Semigroup, Hashable)

data ParseTree = ParseTree SoucModule Imports Body

data CheckedProgram = CheckedProgram SoucModule Imports Body

data ExportDecl = ExportDecl Identifier SoucType
data SoucModule = SoucModule Text [ExportDecl]

data ImportDecl = LibImport Text | RelImport Text deriving Read

type Imports = [ImportDecl]
type Body = [Top_Level_Defn]

newtype Stmts = Stmts [Stmt] deriving Eq

newtype SoucKind = SoucKind Word deriving (Eq, Ord)

-- allowed type names are single chars like 'A'
-- or 'T' followed by an increasing number (T0, T1, ...)
data TypeVar = TypeVar (Either Word Char) SoucKind deriving (Eq, Ord)

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

data TypeError = TypeMismatch SoucType SoucType
               | MultipleDeclarations Identifier
               | Undeclared Identifier
               | UnknownData Text
               | ExportedButNotDefined Bound
    deriving (Eq)

data Param = Param Identifier (Maybe SoucType) deriving Eq


data Top_Level_Defn = Top_Level_Const_Defn Identifier (Maybe SoucType) ASTree
                    | FuncDefn Identifier Param (Maybe SoucType) Stmts
                    | ShortFuncDefn Identifier Param (Maybe SoucType) ASTree
                    | SubDefn Identifier (Maybe Param) (Maybe SoucType) Stmts
                    | MainDefn (Maybe Param) (Maybe SoucType) Stmts
                    deriving Eq

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
          deriving Eq

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Signed ASTree SoucType
            | Leaf Term
         deriving Eq


data Term = LitInt Integer
          | LitChar Char
          | LitString Text
          | Var Identifier
          | Constructor Text
    deriving Eq

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
              | Comma
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

instance TextShow Identifier where
    showb (Identifier i) = TextShow.fromText i

instance Show Identifier where
    show = TextShow.toString . showb

instance TextShow CheckedProgram where
    showb (CheckedProgram m imports body) =
        "CheckedProgam " <> showb m <> " uses " <> showb imports <>
        " : " <> showb body

instance TextShow ExportDecl where
    showb (ExportDecl name t) = showb name <> ": " <> showb t

instance TextShow SoucModule where
    showb (SoucModule name exports) = "souc module: " <> showb name <> " exports: " <> showb exports

instance TextShow ImportDecl where
    showb = \case
        LibImport name -> "lib import " <> showb name
        RelImport name -> "rel import " <> showb name

instance TextShow Stmts where
    showb (Stmts stmts) = "Stmts: " <> showb stmts

instance TextShow TypeVar where
    showb = \case
        TypeVar (Left num) k -> showb num <> ":: " <> showb k
        TypeVar (Right c) k -> showb c <> ":: " <> showb k

instance TextShow SoucKind where
    showb (SoucKind k) = "*" <>
        TextShow.fromText (Text.replicate (fromIntegral k) " => *")

instance TextShow Bound where
    showb (Bound (Identifier i) t) = "Bound " <> showb i <> ": " <> showb t

instance TextShow TypeError where
    showb = \case
        TypeMismatch t0 t1 -> "mismatch: expected " <> showb t0 <> " but got " <> showb t1
        MultipleDeclarations name -> "multiple declarations of " <> showb name
        Undeclared name -> "undeclared " <> showb name
        UnknownData name -> "unknown data constructor: " <> showb name
        ExportedButNotDefined name -> "declared " <> showb name <> " was not defined"

instance TextShow Param where
    showb = \case
        Param p (Just t) -> "param: " <> showb p <> ": " <> showb t
        Param p Nothing -> "param: " <> showb p

instance TextShow Top_Level_Defn where
    showb = \case
        Top_Level_Const_Defn name (Just t) expr -> mconcat
            ["const: ", showb name, ": ", showb t, " = ", showb expr]
        Top_Level_Const_Defn name Nothing expr -> mconcat
            ["const: ", showb name, " = ", showb expr]
        FuncDefn name param (Just t) stmts -> mconcat
            ["fn: ", showb name, "(", showb param, "): ", showb t,
             " = ", showb stmts]
        FuncDefn name param Nothing stmts -> mconcat
            ["fn: ", showb name, "(", showb param
            , " = ", showb stmts]
        ShortFuncDefn name param (Just t) expr -> mconcat
            ["fn: ", showb name, "(", showb param, "): ", showb t,
             " = ", showb expr]
        ShortFuncDefn name param Nothing expr -> mconcat
            ["fn: ", showb name, "(", showb param, ") = ", showb expr]
        SubDefn name m_param m_t stmts -> mconcat
            ["sub: ", showb name, "(", showb m_param, "): ", showb m_t,
             " = ", showb stmts]
        MainDefn m_param m_t stmts -> mconcat
            ["main(", showb m_param, "): ", showb m_t, " = ", showb stmts]

instance TextShow Stmt where
    showb = \case
        Stmt_While expr stmts ->
            "while " <> showb expr <> " do " <> showb stmts
        Stmt_Until expr stmts ->
            "until " <> showb expr <> " do " <> showb stmts
        Stmt_If expr stmts (Just else_stmts) ->
            "if " <> showb expr <> " do " <> showb stmts <>
            " else " <> showb else_stmts
        Stmt_If expr stmts Nothing ->
            "if " <> showb expr <> " do " <> showb stmts
        Stmt_Unless expr stmts (Just else_stmts) ->
            "unless " <> showb expr <> " do " <> showb stmts <>
            " else " <> showb else_stmts
        Stmt_Unless expr stmts Nothing ->
            "unless " <> showb expr <> " do " <> showb stmts
        Stmt_Sub_Call name (Just expr) ->
            "call " <> showb name <> " " <> showb expr
        Stmt_Sub_Call name Nothing ->
            "call " <> showb name
        Stmt_Postfix_Oper name op ->
            showb name <> showb op
        Stmt_Const_Assign name (Just t) expr ->
            showb name <> ": " <> showb t <> " = " <> showb expr
        Stmt_Const_Assign name Nothing expr ->
            showb name <> " = " <> showb expr
        Stmt_Var_Assign name (Just t) expr ->
            showb name <> ": " <> showb t <> " <- " <> showb expr
        Stmt_Var_Assign name Nothing expr ->
            showb name <> " <- " <> showb expr
        Stmt_Var_Reassign name expr ->
            showb name <> " <-- " <> showb expr
        Stmt_Return (Just expr) ->
            "return" <> showb expr
        Stmt_Return Nothing ->
            "return"

instance TextShow ASTree where
    showb = \case
        Branch oper left right -> mconcat
            ["(", showb oper, " ", showb left, " ", showb right, ")"]
        Twig oper tree -> mconcat
            ["(", showb oper, " ", showb tree, ")"]
        Signed tree tree_t -> mconcat
            [showb tree, ": ", showb tree_t]
        Leaf val -> showb val

instance TextShow Term where
    showb = \case
        LitInt i -> showb i
        LitChar c -> showb c
        LitString s -> showb s
        Var v -> showb v
        Constructor name -> showb name

instance TextShow Operator where
    showb = \case
        Plus            ->  "+"
        Minus           ->  "-"
        Splat           ->  "*"
        FieldDiv        ->  "/"
        FloorDiv        ->  "//"
        Modulo          ->  "%"
        Hihat           ->  "^"
        Equals          ->  "=="
        Combine         ->  "<>"
        NotEquals       ->  "=/="
        RegexMatch      ->  "=~"
        GreaterThan     ->  ">"
        LesserThan      ->  "<"
        And             ->  "&&" -- FIXME
        Or              ->  "||" -- FIXME
        Xor             ->  "><"
        In              ->  ">|#|<"
        Comma           ->  ","
        Iff             ->  "?"
        FromMaybe       ->  "??"
        Prepend         ->  ">>"
        Append          ->  "<<"
        Index           ->  "#"
        Lookup          ->  "##"
        Apply           ->  "~&"
        FlipApply       ->  "&"
        Map             ->  "<~&>"
        FlipMap         ->  "<&>"
        Applicative     ->  "<~*>"
        FlipApplicative ->  "<*>"
        SequenceRight   ->  "*>"
        SequenceLeft    ->  "<*"
        BindRight       ->  ">>="
        BindLeft        ->  "=<<"

instance Show Operator where
    show = TextShow.toString . showb

instance TextShow PrefixOperator where
    showb = \case
        Deref    -> "!"
        GetAddr  -> "@"
        Negate   -> "~"
        ToString -> "$"
        Pure     -> "^*^"
        Join     -> ">*<"

instance TextShow SoucType where
    showb = \case
        SoucIO -> "IO"
        SoucFn t0 t1 -> showb t0 <> " -> " <> showb t1
        SoucRoutn t -> showb t <> " -> IO"
        SoucMaybe t -> "?" <> showb t
        SoucList t -> "[" <> showb t <> "]"
        SoucPair t0 t1 -> showb t0 <> " & " <> showb t1
        SoucEither t0 t1 -> showb t0 <> " | " <> showb t1
        SoucType t _ -> showb t
        SoucTypeVar (TypeVar (Left  v) _) -> "T" <> showb v
        SoucTypeVar (TypeVar (Right v) _) -> showb v
        SoucTypeConstructor t _ ts -> showb t <> "(" <> showb ts <> ")"
