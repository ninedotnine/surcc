import CodeGen
import SouC_Types

prog :: Program
prog = Program Nothing [] [Top_Level_Const_Defn (Identifier "x") (Raw_Expr "42")]

main :: IO ()
main = do
    print $ generate prog
