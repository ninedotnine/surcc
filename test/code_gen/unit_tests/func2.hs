import CodeGen
import SouC_Types

prog :: Program
prog = Program Nothing [] [ShortFuncDefn (Identifier "f") [Identifier "x"] (Raw_Expr "42")]

main :: IO ()
main = do
    print $ generate prog
