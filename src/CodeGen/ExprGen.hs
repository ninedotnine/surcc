module CodeGen.ExprGen (
    generate_expr,
    generate_identifier
) where

import Parser.ExprParser (
    ASTree(..),
    Term(..),
    Operator(..),
    PrefixOperator(..)
    )

import Builtins (gen_builtin_data, gen_builtin_identifier)
import Common

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

generate_expr :: ASTree -> Text
generate_expr (Leaf e) = generate_term e
generate_expr (Signed e _) = generate_expr e
generate_expr (Twig op e) = generate_prefix_expr op <> generate_expr e
generate_expr (Branch op x y) = case op of
    Plus              ->  gen_call "_souc_sum(" x y
    Minus             ->  gen_call "_souc_difference(" x y
    Splat             ->  gen_call "_souc_product(" x y
    FieldDiv          ->  gen_call "_souc_quotient(" x y
    FloorDiv          ->  undefined
    Modulo            ->  gen_call "_souc_remainder(" x y
    Hihat             ->  undefined -- FIXME C doesn't have ^
    Equals            ->  gen_call "_souc_is_equal_integer(" x y
    NotEquals         ->  gen_call "_souc_is_unequal_integer(" x y
    RegexMatch        ->  undefined
    GreaterThan       ->  gen_call "_souc_is_greater(" x y
    LesserThan        ->  gen_call "_souc_is_lesser(" x y
    And               ->  gen_call "_souc_conjunction(" x y
    Or                ->  gen_call "_souc_disjunction(" x y
    Xor               ->  undefined
    In                ->  undefined
    Tuple             ->  gen_call "_souc_tuple(" x y
    Iff               ->  undefined
    FromMaybe         ->  undefined
    Prepend           ->  undefined
    Append            ->  undefined
    Combine           ->  undefined
    Index             ->  undefined
    Lookup            ->  undefined
    Apply             ->  generate_expr x <> "(" <> generate_expr y <> ") "
    FlipApply         ->  generate_expr y <> "(" <> generate_expr x <> ") "
    Map               ->  undefined
    FlipMap           ->  undefined
    Applicative       ->  undefined
    FlipApplicative   ->  undefined
    SequenceRight     ->  undefined
    SequenceLeft      ->  undefined
    BindRight         ->  undefined
    BindLeft          ->  undefined

gen_call :: Text -> ASTree -> ASTree -> Text
gen_call s x y = s <> generate_expr x <> "," <> generate_expr y <> ")"

generate_term :: Term -> Text
generate_term (LitInt l) = "(union _souc_obj) { ._souc_int = " <> Text.pack (show l) <> "}"
generate_term (LitChar c) = "\'" <> Text.singleton c <> "\'"
generate_term (LitString s) = "(union _souc_obj) { ._souc_str = \"" <> s <> "\"}"
generate_term (Var i) = generate_identifier i
generate_term (Constructor s) = case gen_builtin_data s of
    Just output -> output
    Nothing -> "45" -- fixme hehe

generate_identifier :: Identifier -> Text
generate_identifier i = fromMaybe (prepend i) (gen_builtin_identifier i) where
    prepend (Identifier name) = "_souc_user_" <> name


generate_prefix_expr :: PrefixOperator -> Text
generate_prefix_expr GetAddr    = "&"
generate_prefix_expr Deref      = "*"
generate_prefix_expr Negate     = "-"
generate_prefix_expr ToString   = undefined
generate_prefix_expr Pure       = undefined
generate_prefix_expr Join       = undefined
