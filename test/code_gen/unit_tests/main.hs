import CodeGen
import SouC_Types

prog :: Program
prog = Program Nothing [] [SubDefn (Identifier "main") Nothing []]

main :: IO ()
main = do
    print $ generate prog
