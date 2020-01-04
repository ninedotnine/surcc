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
              | Combine
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

oper_to_char :: Operator -> Char
oper_to_char Plus   = '+'
oper_to_char Minus  = '-'
oper_to_char Splat  = '*'
oper_to_char Divide = '/'
oper_to_char Modulo = '%'
oper_to_char Hihat  = '^'
oper_to_char Combine  = 'm'

instance Show Operator where
    show x = [oper_to_char x]

get_prec :: Operator -> Precedence
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

pre_oper_to_char :: PrefixOperator -> Char
pre_oper_to_char Deref   = '!'
pre_oper_to_char GetAddr  = '@'
pre_oper_to_char Negate  = '~'
pre_oper_to_char ToString = '$'

instance Show PrefixOperator where
    show x = [pre_oper_to_char x]

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
