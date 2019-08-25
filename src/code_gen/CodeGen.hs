module CodeGen where

import SouC_Types

generate :: Program -> String
-- generate (Program name imports body) =
generate (Program _ _ body) = concat $ map generate_top_level body

generate_top_level :: Top_Level_Defn -> String
generate_top_level (Top_Level_Const_Defn name raw_expr) = "const int " ++ value name ++ " = " ++ value raw_expr ++ ";"
generate_top_level (FuncDefn name param stmts) = "int " ++ value name ++ "(" ++ show param ++ ") {" ++ body ++ "}"
    where body = generate_stmts stmts
generate_top_level (ShortFuncDefn name param raw_expr) = "int " ++ value name ++ "(" ++ show param  ++ ") { return " ++ value raw_expr ++ "; }"
generate_top_level (SubDefn name m_param stmts) = "void " ++ value name ++ "(" ++ param ++ ") { " ++ body ++ "}"
    where
        param = if m_param == Nothing then "void" else show m_param
        body = generate_stmts stmts


generate_stmts :: Stmts -> String
generate_stmts stmts = concat $ map generate_stmt stmts

generate_stmt (Stmt_Return m_raw_expr) = "return " ++ raw_expr ++ "; " where
    raw_expr = case m_raw_expr of
        Nothing -> ""
        Just e -> value e
generate_stmt stmt = undefined -- FIXME
{-
Stmt_While Raw_Expr Stmts
          | Stmt_If Raw_Expr Stmts (Maybe Stmts)
          | Stmt_Sub_Call Identifier (Maybe Raw_Expr)
          | Stmt_Postfix_Oper Identifier String
          | Stmt_Const_Assign Identifier Raw_Expr
          | Stmt_Var_Assign Identifier Raw_Expr
          | Stmt_Return (Maybe Raw_Expr)
          deriving (Read, Show)

-}
