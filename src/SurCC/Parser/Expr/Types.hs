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
import SurCC.Common.SoucTypes (SoucType(..))

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
get_prec = Precedence <$> \case
    Comma          -> 10
    Iff            -> 20
    And            -> 30
    Or             -> 30
    Xor            -> 30
    Equals         -> 40
    NotEquals      -> 40
    RegexMatch     -> 40
    GreaterThan    -> 50
    LesserThan     -> 50
    BindRight      -> 60
    BindLeft       -> 60
    Applicative    -> 70
    FlipApplicative -> 70
    SequenceRight  -> 70
    SequenceLeft   -> 70
    Map            -> 80
    FlipMap        -> 80
    Apply          -> 90
    FlipApply      -> 90
    Plus           -> 130
    Minus          -> 130
    Splat          -> 140
    FieldDiv       -> 140
    FloorDiv       -> 140
    Hihat          -> 150
    FromMaybe      -> 160
    Prepend        -> 170
    Append         -> 170
    Index          -> 180
    Lookup         -> 190
    In             -> 200
    Combine        -> 210

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Oper_Stack = Oper_Stack [StackOp]
newtype Tree_Stack = Tree_Stack [ExprTree]
newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)
-- data Stack_State = Stack_State {
--     get_op_stack :: Oper_Stack,
--     get_tree_stack :: Tree_Stack,
--     get_tightness :: Tightness,
-- }

type ShuntingYardParser t = Parsec.Parsec Text Stack_State t
