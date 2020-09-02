-- all the types needed to implement shunting yard
module Parser.Expr.ExprTypes (
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
import Data.Word (Word8)

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Leaf Term
         deriving (Show, Eq)

newtype Precedence = Precedence Word8 deriving (Eq, Ord)

data Term = LitInt Integer
          | LitChar Char
          | LitBool Bool
          | LitString String
          | Var String (Maybe String)
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
get_prec Apply          = Precedence 10
get_prec FlipApply      = Precedence 10
get_prec Iff            = Precedence 20
get_prec Tuple          = Precedence 30
get_prec BindRight      = Precedence 40
get_prec BindLeft       = Precedence 40
get_prec And            = Precedence 50
get_prec Or             = Precedence 50
get_prec Xor            = Precedence 50
get_prec Applicative    = Precedence 60
get_prec FlipApplicative = Precedence 60
get_prec SequenceRight  = Precedence 60
get_prec SequenceLeft   = Precedence 60
get_prec Equals         = Precedence 70
get_prec NotEquals      = Precedence 70
get_prec RegexMatch     = Precedence 70
get_prec GreaterThan    = Precedence 80
get_prec LesserThan     = Precedence 80
get_prec Map            = Precedence 110
get_prec FlipMap        = Precedence 110
get_prec Plus           = Precedence 130
get_prec Minus          = Precedence 130
get_prec Splat          = Precedence 140
get_prec Divide         = Precedence 140
get_prec FloorDiv       = Precedence 140
get_prec Modulo         = Precedence 140
get_prec Hihat          = Precedence 150
get_prec FromMaybe      = Precedence 160
get_prec Prepend        = Precedence 170
get_prec Append         = Precedence 170
get_prec Index          = Precedence 180
get_prec Lookup         = Precedence 190
get_prec In             = Precedence 200
get_prec Combine        = Precedence 210

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
