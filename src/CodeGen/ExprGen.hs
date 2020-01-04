module ExprGen (
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
generate_oper Plus   = "+"
generate_oper Minus  = "-"
generate_oper Splat  = "*"
generate_oper Divide = "/"
generate_oper Modulo = "%"
generate_oper Hihat  = undefined -- FIXME C doesn't have ^
generate_oper Combine  = "<>"

generate_prefix_expr :: PrefixOperator -> String
generate_prefix_expr GetAddr = "&"
generate_prefix_expr Deref = "*"
generate_prefix_expr Negate = "-"
generate_prefix_expr ToString = undefined
