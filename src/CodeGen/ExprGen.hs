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

generate_expr :: ASTree -> String
generate_expr (Leaf e) = generate_term e
generate_expr (Signed e _) = generate_expr e
generate_expr (Twig op e) = generate_prefix_expr op ++ generate_expr e
generate_expr (Branch op x y) = case op of
    Plus              ->  "_souc_sum(" ++ generate_expr x ++ "," ++ generate_expr y ++ ")"
    Minus             ->  gen_simple_op x "-" y
    Splat             ->  gen_simple_op x "*" y
    Divide            ->  gen_simple_op x "/" y
    FloorDiv          ->  undefined
    Modulo            ->  gen_simple_op x "%" y
    Hihat             ->  undefined -- FIXME C doesn't have ^
    Equals            ->  "_souc_is_equal(" ++ generate_expr x ++ "," ++ generate_expr y ++ ")"
    NotEquals         ->  gen_simple_op x "!=" y
    RegexMatch        ->  undefined
    GreaterThan       ->  gen_simple_op x ">" y
    LesserThan        ->  "_souc_is_lesser(" ++ generate_expr x ++ "," ++ generate_expr y ++ ")"
    And               ->  gen_simple_op x "&&" y
    Or                ->  gen_simple_op x "||" y
    Xor               ->  undefined
    In                ->  undefined
    Tuple             ->  "_souc_tuple(" ++ generate_expr x ++ "," ++
                            generate_expr y ++ ")"
    Iff               ->  undefined
    FromMaybe         ->  undefined
    Prepend           ->  undefined
    Append            ->  undefined
    Combine           ->  undefined
    Index             ->  undefined
    Lookup            ->  undefined
    Apply             ->  generate_expr x ++ "(" ++ generate_expr y ++ ") "
    FlipApply         ->  generate_expr y ++ "(" ++ generate_expr x ++ ") "
    Map               ->  undefined
    FlipMap           ->  undefined
    Applicative       ->  undefined
    FlipApplicative   ->  undefined
    SequenceRight     ->  undefined
    SequenceLeft      ->  undefined
    BindRight         ->  undefined
    BindLeft          ->  undefined

gen_simple_op :: ASTree -> String -> ASTree -> String
gen_simple_op x s y = generate_expr x <> s <> generate_expr y

generate_term :: Term -> String
generate_term (LitInt l) = "(union _souc_obj) { ._souc_int = " ++ show l ++ "}"
generate_term (LitChar c) = "\'" ++ c : "\'"
generate_term (LitString s) = "(union _souc_obj) { ._souc_str = " ++ show s ++ "}"
generate_term (Var i) = generate_identifier i
generate_term (Constructor s) = case gen_builtin_data s of
    Just output -> output
    Nothing -> "45" -- fixme hehe

generate_identifier :: Identifier -> String
generate_identifier i = fromMaybe (prepend i) (gen_builtin_identifier i) where
    prepend (Identifier name) = "_souc_user_" ++ name


generate_prefix_expr :: PrefixOperator -> String
generate_prefix_expr GetAddr    = "&"
generate_prefix_expr Deref      = "*"
generate_prefix_expr Negate     = "-"
generate_prefix_expr ToString   = undefined
generate_prefix_expr Pure       = undefined
generate_prefix_expr Join       = undefined
