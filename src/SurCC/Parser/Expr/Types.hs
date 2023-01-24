-- all the types needed to implement shunting yard
module SurCC.Parser.Expr.Types (
    ShuntingYardParser,
    ExprTree(..),
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
    RawExpr(..),
) where

import Data.Text (Text)
import Data.Word (Word8)
import Text.Parsec qualified as Parsec

import SurCC.Common

data RawExpr = RawExpr Text deriving (Show)

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
get_prec = \case
    Apply          -> Precedence 10
    FlipApply      -> Precedence 10
    Iff            -> Precedence 20
    Comma          -> Precedence 30
    BindRight      -> Precedence 40
    BindLeft       -> Precedence 40
    And            -> Precedence 50
    Or             -> Precedence 50
    Xor            -> Precedence 50
    Applicative    -> Precedence 60
    FlipApplicative -> Precedence 60
    SequenceRight  -> Precedence 60
    SequenceLeft   -> Precedence 60
    Equals         -> Precedence 70
    NotEquals      -> Precedence 70
    RegexMatch     -> Precedence 70
    GreaterThan    -> Precedence 80
    LesserThan     -> Precedence 80
    Map            -> Precedence 110
    FlipMap        -> Precedence 110
    Plus           -> Precedence 130
    Minus          -> Precedence 130
    Splat          -> Precedence 140
    FieldDiv       -> Precedence 140
    FloorDiv       -> Precedence 140
    Modulo         -> Precedence 140
    Hihat          -> Precedence 150
    FromMaybe      -> Precedence 160
    Prepend        -> Precedence 170
    Append         -> Precedence 170
    Index          -> Precedence 180
    Lookup         -> Precedence 190
    In             -> Precedence 200
    Combine        -> Precedence 210

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Oper_Stack = Oper_Stack [StackOp]
newtype Tree_Stack = Tree_Stack [ExprTree]
newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)

type ShuntingYardParser t = Parsec.Parsec Text Stack_State t
