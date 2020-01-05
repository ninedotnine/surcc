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
              | Modulo
              | Hihat
              | Equals
              | Combine
              | Amper
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
    show Plus   = "+"
    show Minus  = "-"
    show Splat  = "*"
    show Divide = "/"
    show Modulo = "%"
    show Hihat  = "^"
    show Equals = "=="
    show Combine  = "<>"
    show Amper = "&"

get_prec :: Operator -> Precedence
get_prec Amper  = Precedence 4
get_prec Equals = Precedence 5
get_prec Plus   = Precedence 6
get_prec Minus  = Precedence 6
get_prec Splat  = Precedence 7
get_prec Divide = Precedence 7
get_prec Modulo = Precedence 7
get_prec Hihat  = Precedence 8
get_prec Combine  = Precedence 8

data PrefixOperator = Deref
                    | GetAddr
                    | Negate
                    | ToString
                    deriving Eq

instance Show PrefixOperator where
    show Deref   = "!"
    show GetAddr = "@"
    show Negate  = "~"
    show ToString = "$"

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
