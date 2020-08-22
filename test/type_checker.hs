{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import TypeChecker.TypeChecker
import Common
import Parser.ExprParser (ASTree(..), Term(..))

import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure)
import Data.String (IsString(..))

-- import System.IO

tmpdir :: FilePath
tmpdir = "/tmp/souc_type_checker_test/"


type Test = (Program, Either TypeError CheckedProgram, String)

tests :: [Test]
tests = [
    (conster, conster_checked, "conster"),
    (int, int_checked, "int"),
    (borked, borked_checked, "borked")
    ]

main :: IO ()
main = do
    System.Directory.createDirectoryIfMissing True tmpdir
    putStrLn "=== testing type-checker"
    mapM_ test tests
    putStrLn "all type-checker tests passed :^)"

instance Eq CheckedProgram where
    CheckedProgram m0 i0 b0 == CheckedProgram m1 i1 b1 =
        m0 == m1 && i0 == i1 && b0 == b1

instance Eq ModuleName where
    ModuleName s0 == ModuleName s1 = s0 == s1

instance Eq Import where
    Import s0 == Import s1 = s0 == s1

instance IsString TypeName where
    fromString = TypeName

test :: Test -> IO ()
test (prog, expected, name) = do
    putStr name >> putStr "... "
--     print prog
    if expected == type_check prog
        then putStrLn "OK."
        else putStrLn "FAILED! bad result" >> exitFailure


conster :: Program
conster =  Program Nothing [] [
    Top_Level_Const_Defn (Identifier "x") Nothing (Leaf (LitInt 42))]

conster_checked :: Either TypeError CheckedProgram
conster_checked = Right $ CheckedProgram Nothing [] [
    Top_Level_Const_Defn (Identifier "x") Nothing (Leaf (LitInt 42))]

int :: Program
int =  Program Nothing [] [
    Top_Level_Const_Defn (Identifier "x") (Just (TypeName "Int")) (Leaf (LitInt 42))]

int_checked :: Either TypeError CheckedProgram
int_checked = Right $ CheckedProgram Nothing [] [
    Top_Level_Const_Defn (Identifier "x") (Just (TypeName "Int")) (Leaf (LitInt 42))]

borked :: Program
borked = Program Nothing [] [
    Top_Level_Const_Defn (Identifier "x") (Just (TypeName "Int")) (Leaf (LitChar 'a'))]

borked_checked :: Either TypeError CheckedProgram
borked_checked = Left (TypeError (TypeName "Int") (TypeName "Char"))
