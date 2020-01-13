module CodeGen.ExprGen (
    generate_expr
) where

import ExprParser

generate_expr :: ASTree -> String
generate_expr (Leaf t) = generate_term t
generate_expr (Twig op t) = generate_prefix_expr op ++ generate_expr t
generate_expr (Branch op x y) = generate_expr x ++ generate_oper op ++ generate_expr y

generate_term :: Term -> String
generate_term (Lit l) = show l
generate_term (CharLit c) = "\'" ++ c : "\'"
generate_term (StringLit s) = show s
generate_term (Var v) = v

generate_oper :: Operator -> String
generate_oper Plus              = "+"
generate_oper Minus             = "-"
generate_oper Splat             = "*"
generate_oper Divide            = "/"
generate_oper FloorDiv          = undefined
generate_oper Modulo            = "%"
generate_oper Hihat             = undefined -- FIXME C doesn't have ^
generate_oper Equals            = "=="
generate_oper NotEquals         = "!="
generate_oper RegexMatch        = undefined
generate_oper GreaterThan       = ">"
generate_oper LesserThan        = "<"
generate_oper Apply             = undefined -- this seems like a challenge to be handled elsewhere
generate_oper And               = "&&"
generate_oper Or                = "||"
generate_oper Xor               = undefined
generate_oper In                = undefined
generate_oper Tuple             = undefined
generate_oper Iff               = undefined
generate_oper FromMaybe         = undefined
generate_oper Prepend           = undefined
generate_oper Append            = undefined
generate_oper Combine           = undefined
generate_oper Index             = undefined
generate_oper Lookup            = undefined
generate_oper FlipApply         = undefined
generate_oper Map               = undefined
generate_oper FlipMap           = undefined
generate_oper Applicative       = undefined
generate_oper FlipApplicative   = undefined
generate_oper SequenceRight     = undefined
generate_oper SequenceLeft      = undefined
generate_oper BindRight         = undefined
generate_oper BindLeft          = undefined

generate_prefix_expr :: PrefixOperator -> String
generate_prefix_expr GetAddr    = "&"
generate_prefix_expr Deref      = "*"
generate_prefix_expr Negate     = "-"
generate_prefix_expr ToString   = undefined
generate_prefix_expr Pure       = undefined
generate_prefix_expr Join       = undefined
