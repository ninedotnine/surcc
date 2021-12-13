module Parser.SouCParser (
    parse
) where

import Data.List.NonEmpty ( NonEmpty(..) )
import qualified Data.Map.Strict as Map (Map, fromList)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec hiding (space, spaces, string, parse)


import Common
import Parser.Common
import Parser.Basics (param, end_block_named, add_to_bindings, identifier)
import Common.Parsing
import Parser.Expr.Raw  (raw_expr)
import Parser.SouC_Stmts (stmt_block, stmt_block_with_param)
import Parser.ExprParser (parse_expression)
import Parser.TabChecker (check_tabs)

parse :: SourceName -> (SoucModule, [ImportDecl], Text, SourcePos) -> Either ParseError ParseTree
parse source_name ((SoucModule name exports), imps, input, pos) = do
    check_tabs source_name input
    body <- runParser (souCParser pos) (start_state name imps) source_name input
    pure $ ParseTree (SoucModule name exports) imps body


type ModuleName = Text
start_state :: ModuleName -> [ImportDecl] -> ParserState
start_state name imps = (0, binds:|[])
    where
        make_identifier :: ImportDecl -> Identifier
        make_identifier = \case
            LibImport n -> Identifier n
            RelImport n -> Identifier n
        binds = Map.fromList (zip ids (repeat Immut))
        ids = (Identifier name) : map make_identifier imps


souCParser :: SourcePos -> SouCParser [TopLevelDefn]
souCParser pos = do
    setPosition pos
    _ <- many (pragma) *> skipMany endline -- FIXME do something with pragmas
    code <- parseDefs
    eof
    pure $ code

parseDefs :: SouCParser [TopLevelDefn]
parseDefs = do
    defns <- many (parse_def <* many endline)
    pure defns

parse_def :: SouCParser TopLevelDefn
parse_def = do
    defn <- main_defn
            <|> top_level_const
            <|> top_level_proc
            <|> (pragma *> skipMany endline *> parse_def)
            <?> "top-level definition"
    pure defn

main_defn :: SouCParser TopLevelDefn
main_defn = do
    _ <- string "main("
    add_to_bindings (Identifier "main") Immut
    p <- optionMaybe param <* char ')'
    sig <- optionMaybe type_signature
    _ <- spaces <* char '=' <* spaces <* string "do" <* endline
    stmts <- stmt_block
    optional $ end_block_named (Identifier "main")
    pure $ MainDefn p sig stmts

top_level_const :: SouCParser TopLevelDefn
top_level_const = do
    name <- try (identifier <* lookAhead (char ':' <|> char ' '))
    add_to_bindings name Immut
    m_sig <- optional_sig
    _ <- spaces <* char '='
    val <- spaces *> raw_expr
    endline
    case parse_expression val of
        Right expr -> do
            pure $ TopLevelConstDefn name m_sig expr
        Left err -> parserFail $ "invalid expression:\n" ++ show err

top_level_proc :: SouCParser TopLevelDefn
top_level_proc = do
    proc_name <- try (identifier <* char '(')
    add_to_bindings proc_name Immut
    name <- top_level_func proc_name <|> top_level_sub proc_name
    pure name

top_level_func :: Identifier -> SouCParser TopLevelDefn
top_level_func func_name = do
    (p, sig) <- try $ do
        p' <- (param <* char ')')
        sig' <- optionMaybe type_signature
        spaces *> char '=' *> spaces *> notFollowedBy (string "do")
        pure (p', sig')
    long_top_level_func func_name p sig <|> short_top_level_func func_name p sig

short_top_level_func :: Identifier -> Param -> Maybe SoucType -> SouCParser TopLevelDefn
short_top_level_func func_name p sig = do
    body <- raw_expr
    case parse_expression body of
        Right result -> do
            pure $ ShortFuncDefn func_name p sig result
--         Left parse_err -> mergeError (fail "invalid expression") parse_err
        Left err -> fail $ "invalid expression\n" ++ show err

long_top_level_func :: Identifier -> Param -> Maybe SoucType -> SouCParser TopLevelDefn
long_top_level_func func_name p sig = do
    string "fndo" *> endline
    stmts <- stmt_block_with_param (Just p)
    optional $ end_block_named func_name
    pure $ FuncDefn func_name p sig stmts

top_level_sub :: Identifier -> SouCParser TopLevelDefn
top_level_sub sub_name = do
    p <- optionMaybe param <* char ')'
    sig <- optionMaybe type_signature
    spaces *> char '=' *> spaces *> string "do" *> endline
    stmts <- stmt_block_with_param p
    optional $ end_block_named sub_name
    pure $ SubDefn sub_name p sig stmts
