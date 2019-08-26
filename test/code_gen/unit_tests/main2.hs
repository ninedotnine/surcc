import CodeGen
import SouC_Types

prog :: Program
prog = Program Nothing [] [SubDefn (Identifier "main") Nothing [Stmt_Return Nothing]]

main :: IO ()
main = do
    print $ generate prog
