{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
    SoucKind(..),
    SoucType(..),
    Refutable(..),
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
    pattern SoucModuleType,
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

data CheckedProgram = CheckedProgram SurCModule Imports Body

data ExportDecl = ExportDecl Bound

data SurCModule = SurCModule Identifier [ExportDecl]

data ImportDecl = LibImport Text | RelImport Text

type Imports = [ImportDecl]
type Body = [TopLevelDefn]

data Stmts = Stmts [Stmt] (Maybe Return) deriving (Eq, Show)

newtype SoucKind = SoucKind Word deriving (Eq, Ord, Show)

-- allowed type names are single chars like 'A'
-- or 'T' followed by an increasing number (T0, T1, ...)
data TypeVar = TypeVar (Either Word Char) SoucKind deriving (Eq, Ord, Show)

data SoucType = SoucType Text SoucKind
              | SoucTypeConstructor Text SoucKind [SoucType]
              | SoucTypeVar TypeVar
--               | SoucConstrainedType Constraint SoucType
              deriving (Eq,Show,Ord)

-- data Constraint = Instance Text SoucType deriving (Eq)

data TypeDef = EmptyType SoucType
             | UnitType SoucType Identifier
             | SynonymType SoucType SoucType
             | WrapperType SoucType Identifier SoucType
                        -- fixme: a wrapper (a function) is not a term.
             | EnumType SoucType [Identifier]
             | StructType SoucType -- fixme
             | GADType SoucType -- fixme
             deriving (Show)

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

pattern SoucModuleType :: SoucType
pattern SoucModuleType = SoucType "Module" (SoucKind 0)

data Bound = Bound Identifier SoucType deriving Eq

instance Show Bound where
    show (Bound (Identifier i) t) =
        Text.unpack $ i <> ": " <> showt t


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
            deriving Eq

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

instance TextShow Literal where
    showb = \case
        LitInt i -> "int " <> showb i
        LitChar c -> "char " <> showb c
        LitString s -> "string " <> showb s


instance TextShow Pattern where
    showb = \case
        PatLit l -> "lit " <> showb l
        PatConst i -> "= " <> showb i
        PatBinding i -> "binding " <> showb i
--  FIXME do i need these? i do need sub-patterns
--         PatConstant k -> "constant " <> showb k
--         PatConstructor k pats -> mconcat
--             ["constructor ", showb k, " pats: ", showb pats]


instance TextShow Guard where
    showb (Guard cond) = "iff " <> showb cond



instance TextShow Identifier where
    showb (Identifier i) = TextShow.fromText i

instance Show Identifier where
    show = TextShow.toString . showb

instance TextShow CheckedProgram where
    showb (CheckedProgram m imports body) =
        "CheckedProgam " <> showb m <> " uses " <> showb imports <>
        " : " <> showb body

instance TextShow ExportDecl where
    showb (ExportDecl (Bound name t)) = "export: " <> showb name <> ": " <> showb t

instance TextShow SurCModule where
    showb (SurCModule name exports) = "surc module: " <> showb name <> " exports: " <> showb exports

instance TextShow ImportDecl where
    showb = \case
        LibImport name -> "lib import " <> showb name
        RelImport name -> "rel import " <> showb name

instance TextShow Stmts where
    showb (Stmts stmts ret) = "Stmts: " <> showb stmts
                              <> " " <> showb ret

instance TextShow TypeVar where
    showb = \case
        TypeVar (Left num) k -> showb num <> ":: " <> showb k
        TypeVar (Right c) k -> showb c <> ":: " <> showb k

instance TextShow SoucKind where
    showb (SoucKind k) = "*" <>
        TextShow.fromText (Text.replicate (fromIntegral k) " => *")

instance TextShow Bound where
    showb (Bound i t) = "Bound " <> showb i <> ": " <> showb t

instance TextShow TypeError where
    showb = \case
        TypeMismatch t0 t1 -> "mismatch: expected " <> showb t0 <> " but got " <> showb t1
        MultipleDeclarations name -> "multiple declarations of " <> showb name
        MultipleTypeDeclarations name ->
            "multiple declarations of " <> showb name
        Undeclared name -> "undeclared " <> showb name
        BadReassign name -> "cannot reassign immutable: " <> showb name
        MutateImmutable name t -> "cannot mutate immutable: " <> showb name
                                  <> " : " <> showb t
        UnknownData name -> "unknown data constructor: " <> showb name
        UnknownType name -> "unknown type: " <> showb name
        ExportedButNotDefined name -> "declared " <> showb name <> " was not defined"
        ExportedLocal name -> "exported " <> showb name <> " must be global"

instance TextShow Param where
    showb = \case
        Param p (Just t) -> "param: " <> showb p <> ": " <> showb t
        Param p Nothing -> "param: " <> showb p

instance TextShow MainParam where
    showb (MainParam (MainParamStdIn stdin) (MainParamStdOut stdout) (MainParamStdErr stderr) (MainParamProgName progname) (MainParamArgs args) (MainParamEnv env)) =
        let iff b word = if b then word else ""
        in "mainparam packing: [ "
            <> iff stdin "stdin "
            <> iff stdout "stdout "
            <> iff stderr "stderr "
            <> iff progname "progname "
            <> iff args "args "
            <> iff env "env "
            <> "]"

instance TextShow TopLevelDefn where
    showb = \case
        TopLevelConstDefn name (Just t) expr -> mconcat
            ["const: ", showb name, ": ", showb t, " = ", showb expr]
        TopLevelConstDefn name Nothing expr -> mconcat
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
        Stmt_Const_Assign_Static name (Just t) expr ->
            showb name <> ": " <> showb t <> " = " <> showb expr
        Stmt_Const_Assign_Static name Nothing expr ->
            showb name <> " = " <> showb expr
        Stmt_Const_Assign_Dynamic name (Just t) expr ->
            showb name <> ": " <> showb t <> " = " <> showb expr
        Stmt_Const_Assign_Dynamic name Nothing expr ->
            showb name <> " = " <> showb expr
        Stmt_Var_Declare name (Just t) expr ->
            showb name <> ": " <> showb t <> " <- " <> showb expr
        Stmt_Var_Declare name Nothing expr ->
            showb name <> " <- " <> showb expr
        Stmt_Var_Reassign name (Just t) expr ->
            showb name <> ": " <> showb t <> " <-- " <> showb expr
        Stmt_Var_Reassign name Nothing expr ->
            showb name <> " <-- " <> showb expr


instance TextShow Return where
    showb = \case
        Return (Just expr) -> "return" <> showb expr
        Return Nothing -> "return"


instance TextShow ExprTree where
    showb = \case
        Branch oper left right -> mconcat
            ["(", showb oper, " ", showb left, " ", showb right, ")"]
        Twig oper tree -> mconcat
            ["(", showb oper, " ", showb tree, ")"]
        Signed tree tree_t -> mconcat
            [showb tree, ": ", showb tree_t]
        Leaf val -> showb val
        Match expr branches -> mconcat
            ["match ", showb expr, " of ", showb branches]

instance TextShow Term where
    showb = \case
        Lit l -> showb l
        Name n -> showb n

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
        SoucType t _ -> TextShow.fromText t
        SoucTypeVar (TypeVar (Left  v) _) -> "T" <> showb v
        SoucTypeVar (TypeVar (Right v) _) -> TextShow.singleton v
        SoucTypeConstructor t _ ts -> showb t <> "(" <> showb ts <> ")"

instance TextShow TypeDef where
    showb = \case
        EmptyType t -> "empty type " <> showb t
        UnitType t _ -> "unit type " <> showb t
        SynonymType t0 t1 -> "synonym " <> showb t0 <> " = " <> showb t1
        WrapperType t0 _ t1 -> "wrapper " <> showb t0 <> " wraps " <> showb t1
        EnumType t terms -> "enum type " <> showb t <> " = " <> showb terms
        StructType _ -> error "fixme typedef textshow"
        GADType _ -> error "fixme typedef textshow"


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
