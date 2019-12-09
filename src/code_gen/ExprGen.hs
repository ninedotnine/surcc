module ExprGen (
    generate_expr
) where

import ShuntingYard

generate_expr :: ASTree -> String
generate_expr (Leaf t) = generate_term t
generate_expr (Branch op x y) = generate_expr x ++ generate_oper op ++ generate_expr y

generate_term :: Term -> String
generate_term (Lit l) = show l
generate_term (Var v) = v

generate_oper :: Operator -> String
generate_oper Plus   = "+"
generate_oper Minus  = "-"
generate_oper Splat  = "*"
generate_oper Divide = "/"
generate_oper Modulo = "%"
generate_oper Hihat  = undefined -- FIXME C doesn't have ^
