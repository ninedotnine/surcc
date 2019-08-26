import CodeGen
import SouC_Types

empty_parse_tree :: Program
empty_parse_tree = Program Nothing [] []

code = generate empty_parse_tree

main :: IO ()
main = do
    print code
