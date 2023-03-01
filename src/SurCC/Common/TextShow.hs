{-# OPTIONS_GHC -Wno-orphans #-}

module SurCC.Common.TextShow () where

import Data.Function
import Data.Functor

import TextShow (TextShow(..))
import TextShow qualified
import Data.Text qualified as Text

import SurCC.Common
import SurCC.Common.SoucTypes


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

instance TextShow CheckedProgram where
    showb (CheckedProgram m imports _constructors body) =
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
        TypeVar (Left i) -> "T" <> showb i
        TypeVar (Right c) -> TextShow.singleton c

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
        SoucType t -> TextShow.fromText t
        SoucTypeVar v ts -> showb v <> (ts <&> in_parens & mconcat)
        SoucTypeCon c ts -> showb c <> (ts <&> in_parens & mconcat)
        where
            in_parens t = "(" <> showb t <> ")"

instance TextShow TypeDef where
    showb = \case
        EmptyType t -> "empty type " <> showb t
        UnitType t _ -> "unit type " <> showb t
        SynonymType t0 t1 -> "synonym " <> showb t0 <> " = " <> showb t1
        IsomorphismType t0 _ t1 _ -> "wrapper " <> showb t0
                                     <> " wraps " <> showb t1
        EnumType t terms -> "enum type " <> showb t <> " = " <> showb terms
        StructType t fns -> "struct type " <> showb t <>
                            " = {" <> showb fns <> "}"
        GADType _ -> error "fixme typedef textshow"

