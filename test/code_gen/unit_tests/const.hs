import CodeGen
import SouC_Types

prog :: Program
prog = Program Nothing [] [TopLevelConstDefn (Identifier "x") (RawExpr "42")]

main :: IO ()
main = do
    print $ generate prog
