-- all the types needed to implement shunting yard
module Expr.ExprTypes (
    ShuntingYardParser,
    ASTree(..),
    Term(..),
    Operator(..),
    TermToken(..),
    OperToken(..),
    PrefixOperator(..),
    Precedence,
    get_prec,
    StackOp(..),
    Oper_Stack(..),
    Tree_Stack(..),
    Tightness(..),
    Stack_State,
) where

import qualified Text.Parsec as Parsec

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Leaf Term
         deriving Show

newtype Precedence = Precedence Integer deriving (Eq, Ord)

data Term = Lit Integer
          | Var String
          | CharLit Char
          | StringLit String
    deriving Show

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

data TermToken = TermTok Term
               | TightPreOp PrefixOperator
               | SpacedPreOp PrefixOperator
               | LParen
    deriving Show

data OperToken = Oper Operator
               | RParen
               | RParenAfterSpace

data StackOp = StackLParen
             | StackLParenFollowedBySpace
             | StackSpace
             | StackOp Operator
             | StackTightPreOp PrefixOperator
             | StackSpacedPreOp PrefixOperator
             deriving (Show, Eq)

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

get_prec :: Operator -> Precedence
get_prec Iff            = Precedence 1
get_prec Tuple          = Precedence 2
get_prec And            = Precedence 3
get_prec Or             = Precedence 3
get_prec Xor            = Precedence 3
get_prec In             = Precedence 3
get_prec Apply          = Precedence 4
get_prec FlipApply      = Precedence 4
get_prec BindRight      = Precedence 4
get_prec BindLeft       = Precedence 4
get_prec Index          = Precedence 5
get_prec Lookup         = Precedence 5
get_prec Map            = Precedence 5
get_prec FlipMap        = Precedence 5
get_prec Applicative    = Precedence 5
get_prec FlipApplicative = Precedence 5
get_prec SequenceRight  = Precedence 5
get_prec SequenceLeft   = Precedence 5
get_prec Equals         = Precedence 5
get_prec NotEquals      = Precedence 5
get_prec RegexMatch     = Precedence 5
get_prec GreaterThan    = Precedence 5
get_prec LesserThan     = Precedence 5
get_prec Plus           = Precedence 6
get_prec Minus          = Precedence 6
get_prec Splat          = Precedence 7
get_prec Divide         = Precedence 7
get_prec FloorDiv       = Precedence 7
get_prec Modulo         = Precedence 7
get_prec Hihat          = Precedence 8
get_prec FromMaybe      = Precedence 9
get_prec Prepend        = Precedence 10
get_prec Append         = Precedence 10
get_prec Combine        = Precedence 10

data PrefixOperator = Deref
                    | GetAddr
                    | Negate
                    | ToString
                    | Pure
                    | Join
                    deriving Eq

instance Show PrefixOperator where
    show Deref    = "!"
    show GetAddr  = "@"
    show Negate   = "~"
    show ToString = "$"
    show Pure     = "^*^"
    show Join     = ">*<"

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Oper_Stack = Oper_Stack [StackOp] deriving Show
newtype Tree_Stack = Tree_Stack [ASTree] deriving Show
newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)

type ShuntingYardParser t = Parsec.Parsec String Stack_State t
