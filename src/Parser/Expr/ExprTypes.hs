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

import Data.Text (Text)
import Data.Word (Word8)
import qualified Text.Parsec as Parsec

import Common

newtype Precedence = Precedence Word8 deriving (Eq, Ord)

data TermToken = TermTok Term
               | TightPreOp PrefixOperator
               | SpacedPreOp PrefixOperator
               | LParen

data OperToken = Oper Operator
               | RParen
               | RParenAfterSpace

data StackOp = StackLParen
             | StackLParenFollowedBySpace
             | StackSpace
             | StackOp Operator
             | StackTightPreOp PrefixOperator
             | StackSpacedPreOp PrefixOperator
             | StackSig SoucType
             deriving Eq

get_prec :: Operator -> Precedence
get_prec Apply          = Precedence 10
get_prec FlipApply      = Precedence 10
get_prec Iff            = Precedence 20
get_prec Comma          = Precedence 30
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
get_prec FieldDiv       = Precedence 140
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

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Oper_Stack = Oper_Stack [StackOp]
newtype Tree_Stack = Tree_Stack [ASTree]
newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)

type ShuntingYardParser t = Parsec.Parsec Text Stack_State t
