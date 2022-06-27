import CodeGen
import SurC_Types

prog :: Program
prog = Program Nothing [] [FuncDefn (Identifier "f") [Identifier "x"] [Stmt_Return (Just (Raw_Expr "42"))]]

main :: IO ()
main = do
    print $ generate prog
