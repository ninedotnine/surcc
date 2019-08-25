module Main where


import SouC_Types
-- import Text.Parsec.String (parseFromFile)
import Text.Parsec.Error (ParseError)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Parser

{-
parseFromFile :: String -> IO Stmt
parseFromFile file = do
    program  <- readFile file
    case parse undefined  "sup?" program of
        Left e  -> print e >> fail "parse error"
        Right r -> return r

       -}

getFileData :: IO (FilePath, String)
getFileData = getArgs >>= \args -> if length args < 1
    then do
        contents <- getContents
        putStrLn "no file name provided, reading from stdin."
        return ("stdin", contents)
    else let name = head args in do
        contents <- readFile name
        return (name, contents)

outputResult :: FilePath -> Program -> IO ()
outputResult filename (Program name imps body) = do
    putStrLn filename
--     putStr $ (unlines . map show) toks
    putStrLn "------------------ pretty printing ------------------"
    case name of
        Just str -> print $ "importing module: " ++ show str
        Nothing -> return ()
    mapM_ print imps
    mapM_ prettyPrint body
    print_file_contents filename

print_file_contents :: FilePath -> IO ()
print_file_contents filename = do
    putStrLn "-------------------- file contents: --------------------"
    contents <- map ("   "++) . lines <$> readFile filename
    mapM_ putStrLn $ zipWith (++) (map show ([1..]::[Int])) contents


prettyPrint :: Top_Level_Defn -> IO ()
prettyPrint (SubDefn name param stmts) = do
    putStr $ "sub " ++ show name ++ " takes "
    putStrLn $ show param
    putStrLn (unlines (map prettifyStmt stmts))
prettyPrint (FuncDefn name param stmts) = do
    putStrLn $ "fn " ++ show name ++ " takn " ++ show param ++ " " ++ (unlines (map prettifyStmt stmts))
prettyPrint (ShortFuncDefn name param expr) = do
    putStrLn $ "fn" ++ show name ++ " takn " ++ show param ++ " = " ++ show expr
prettyPrint (Top_Level_Const_Defn name val) = do
    putStrLn $ "const " ++ show name ++ " = " ++ show val

prettifyStmt :: Stmt -> String
prettifyStmt stmt = show stmt -- FIXME could be much prettier



main :: IO ()
main = do
    putStrLn "------------------------BEGIN------------------------"
    (filename, input) <- getFileData
    let result = runSouCParser filename input :: Either ParseError Program
    case result of
        Left err -> do
            putStrLn "-------------------- failed parse output:--------------------"
            putStrLn (show err)
            print_file_contents filename
            exitFailure >> return ()
        Right prog -> do
            outputResult filename prog
            exitSuccess >> return ()
