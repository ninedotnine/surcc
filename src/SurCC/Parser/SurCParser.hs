module SurCC.Parser.SurCParser (
    parse
) where

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Map.Strict qualified as Map (Map, fromList)
import Data.Text (Text)
import Data.Text qualified as Text
import Text.Parsec hiding (space, spaces, string, parse)


import SurCC.Common
import SurCC.Parser.Common
import SurCC.Parser.Basics (param, end_block_named, add_to_bindings, identifier)
import SurCC.Common.Parsing
import SurCC.Parser.Expr.Raw  (raw_expr)
import SurCC.Parser.SurCStmts (stmt_block, stmt_block_with_param)
import SurCC.Parser.ExprParser (parse_expression)
import SurCC.Parser.TabChecker (check_tabs)
import SurCC.Parser.TypeDefs (type_def)

parse :: SourceName -> (SurCModule, [ImportDecl], Text, SourcePos) -> Either ParseError ParseTree
parse source_name ((SurCModule name exports), imps, input, pos) = do
    check_tabs source_name input
    (typedefs, body) <- runParser
                            (surc_parser pos)
                            (start_state name imps)
                            source_name
                            input
    pure $ (ParseTree (SurCModule name exports) imps typedefs body)


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


surc_parser :: SourcePos -> SurCParser ([TypeDef], [TopLevelDefn])
surc_parser pos = do
    setPosition pos
    _ <- many (pragma) *> skipMany endline -- FIXME do something with pragmas
    typedefs <- many (type_def <* many endline)
    code <- parseDefs
    eof
    pure $ (typedefs, code)

parseDefs :: SurCParser [TopLevelDefn]
parseDefs = do
    defns <- many (parse_def <* many endline)
    pure defns

parse_def :: SurCParser TopLevelDefn
parse_def = do
    defn <- main_defn
            <|> top_level_const
            <|> top_level_proc
            <|> (pragma *> skipMany endline *> parse_def)
            <?> "top-level definition"
    pure defn

main_defn :: SurCParser TopLevelDefn
main_defn = do
    _ <- string "main("
    add_to_bindings (Identifier "main") Immut
    p <- main_param <* char ')'
    sig <- optionMaybe type_signature
    _ <- spaces <* char '=' <* spaces <* reserved "do" <* endline
    stmts <- stmt_block
    optional $ end_block_named (Identifier "main")
    pure $ MainDefn p sig stmts

main_param :: SurCParser MainParam
main_param = do
    -- FIXME this should match a pattern,
    -- not (any) single identifier
--     name <- choice (string <$>
--                     ["stdin", "stdout", "stderr",
--                      "program_name", "args", "env"])
    p <- optionMaybe $ do
        stdout <- string "stdout"
        _ <- optionMaybe type_signature
        pure stdout
    -- FIXME yeah i'm not even trying
    case p of
        Just _ ->
            pure $ MainParam
                (MainParamStdIn False) (MainParamStdOut True)
                (MainParamStdErr False) (MainParamProgName False)
                (MainParamArgs False) (MainParamEnv False)
        Nothing ->
            pure $ MainParam
                (MainParamStdIn False) (MainParamStdOut False)
                (MainParamStdErr False) (MainParamProgName False)
                (MainParamArgs False) (MainParamEnv False)



top_level_const :: SurCParser TopLevelDefn
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

top_level_proc :: SurCParser TopLevelDefn
top_level_proc = do
    proc_name <- try (identifier <* char '(')
    add_to_bindings proc_name Immut
    name <- top_level_func proc_name <|> top_level_sub proc_name
    pure name

top_level_func :: Identifier -> SurCParser TopLevelDefn
top_level_func func_name = do
    (p, sig) <- try $ do
        p' <- (param <* char ')')
        sig' <- optionMaybe type_signature
        spaces *> char '=' *> spaces *> notFollowedBy (reserved "do")
        pure (p', sig')
    long_top_level_func func_name p sig <|> short_top_level_func func_name p sig

short_top_level_func :: Identifier -> Param -> Maybe SoucType -> SurCParser TopLevelDefn
short_top_level_func func_name p sig = do
    body <- raw_expr
    case parse_expression body of
        Right result -> do
            pure $ ShortFuncDefn func_name p sig result
--         Left parse_err -> mergeError (fail "invalid expression") parse_err
        Left err -> fail $ "invalid expression\n" ++ show err

long_top_level_func :: Identifier -> Param -> Maybe SoucType -> SurCParser TopLevelDefn
long_top_level_func func_name p sig = do
    string "fndo" *> endline
    stmts <- stmt_block_with_param (Just p)
    optional $ end_block_named func_name
    pure $ FuncDefn func_name p sig stmts

top_level_sub :: Identifier -> SurCParser TopLevelDefn
top_level_sub sub_name = do
    p <- optionMaybe param <* char ')'
    sig <- optionMaybe type_signature
    spaces *> char '=' *> spaces *> reserved "do" *> endline
    stmts <- stmt_block_with_param p
    optional $ end_block_named sub_name
    pure $ SubDefn sub_name p sig stmts
