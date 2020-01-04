-- this is an infix expression parser.
-- it can be extended to support operations with arbitrary precedence.
-- it does not make any attempt at associativity, although this is possible.
-- it gives higher precedence to operators which are not separated by spaces.

-- module ShuntingYard (run_shunting_yard, print_shunting_yard, pretty_show) where
module ShuntingYard (
    pretty_show,
    run_shunting_yard,
    print_shunting_yard,
    evaluate,
    eval_show,
    parse_eval_print,
    ASTree(..),
    Term(..),
    Operator(..),
    PrefixOperator(..)
) where


import Control.Monad (when)
import qualified Text.Parsec as Parsec
import Text.Parsec (Parsec, (<|>), (<?>))

-- for trim_spaces
import Data.Char (isSpace)
import Data.Functor ((<&>))
import Data.List (dropWhile, dropWhileEnd)


import Data.Char (ord) -- for evaluate

-- the oper stack is a temporary storage place for opers
-- the tree stack holds the result, the output, as well as being used for
-- intermediate storage
-- ultimately the oper stack should be empty and the tree stack should contain
-- only the complete expression tree

newtype Precedence = Precedence Integer deriving (Eq, Ord)

data Term = Lit Integer
          | Var String
          | CharLit Char
          | StringLit String
    deriving Show

data OperToken = Oper Operator
               | RParen
               | RParenAfterSpace

data TermToken = TermTok Term
               | TightPreOp PrefixOperator
               | SpacedPreOp PrefixOperator
               | LParen
    deriving Show

data ASTree = Branch Operator ASTree ASTree
            | Twig PrefixOperator ASTree
            | Leaf Term
         deriving Show

data StackOp = StackLParen
             | StackLParenFollowedBySpace
             | StackSpace
             | StackOp Operator
             | StackTightPreOp PrefixOperator
             | StackSpacedPreOp PrefixOperator
             deriving (Show, Eq)

data Operator = Plus
              | Minus
              | Splat
              | Divide
              | Modulo
              | Hihat
              | Combine
            deriving Eq

data PrefixOperator = Deref
                    | GetAddr
                    | Negate
                    | ToString
                    deriving Eq

newtype Oper_Stack = Oper_Stack [StackOp] deriving Show
newtype Tree_Stack = Tree_Stack [ASTree] deriving Show
newtype Tightness = Tight Bool deriving Eq

type Stack_State = (Oper_Stack, Tree_Stack, Tightness)

oper_to_char :: Operator -> Char
oper_to_char Plus   = '+'
oper_to_char Minus  = '-'
oper_to_char Splat  = '*'
oper_to_char Divide = '/'
oper_to_char Modulo = '%'
oper_to_char Hihat  = '^'
oper_to_char Combine  = 'm'

instance Show Operator where
    show x = [oper_to_char x]

pre_oper_to_char :: PrefixOperator -> Char
pre_oper_to_char Deref   = '!'
pre_oper_to_char GetAddr  = '@'
pre_oper_to_char Negate  = '~'
pre_oper_to_char ToString = '$'

instance Show PrefixOperator where
    show x = [pre_oper_to_char x]

get_prec :: Operator -> Precedence
get_prec Plus   = Precedence 6
get_prec Minus  = Precedence 6
get_prec Splat  = Precedence 7
get_prec Divide = Precedence 7
get_prec Modulo = Precedence 7
get_prec Hihat  = Precedence 8
get_prec Combine  = Precedence 8


-- functions to get the current state
get_op_stack :: Parsec String Stack_State Oper_Stack
get_op_stack = do
    (stack, _, _) <- Parsec.getState
    return stack

get_tree_stack :: Parsec String Stack_State Tree_Stack
get_tree_stack = do
    (_, stack, _) <- Parsec.getState
    return stack

get_tightness :: Parsec String Stack_State Tightness
get_tightness = do
    (_, _, tightness) <- Parsec.getState
    return tightness

-- functions to change or set the current state
oper_stack_push :: StackOp -> Parsec String Stack_State ()
oper_stack_push op =
    Parsec.modifyState (\(Oper_Stack ops, terms, b) -> (Oper_Stack (op:ops), terms, b))

oper_stack_set :: [StackOp] -> Parsec String Stack_State ()
oper_stack_set tokes = Parsec.modifyState (\(_,s2,b) -> (Oper_Stack tokes, s2, b))

tree_stack_push :: ASTree -> Parsec String Stack_State ()
tree_stack_push tree =
    Parsec.modifyState (\(ops, Tree_Stack vals, b) -> (ops, Tree_Stack (tree:vals), b))

tree_stack_pop :: Parsec String Stack_State ASTree
tree_stack_pop = do
    (opers, vals, b) <- Parsec.getState
    case vals of
        Tree_Stack (v:vs) -> do
            Parsec.setState (opers, Tree_Stack vs, b)
            return v
        Tree_Stack _ -> Parsec.unexpected "?? did i expect a term?"


set_spacing_tight :: Bool -> Parsec String Stack_State ()
set_spacing_tight b = Parsec.modifyState (\(s1,s2,_) -> (s1, s2, Tight b))

-- functions that build the ASTree
make_branch :: Operator -> [StackOp] -> Parsec String Stack_State ()
make_branch op tokes = do
    r <- tree_stack_pop
    l <- tree_stack_pop
    tree_stack_push (Branch op l r)
    oper_stack_set tokes

make_twig :: PrefixOperator -> [StackOp] -> Parsec String Stack_State ()
make_twig op tokes = do
    tree <- tree_stack_pop
    tree_stack_push (Twig op tree)
    oper_stack_set tokes

-- simple spacing-related parsers
respect_spaces :: Parsec String Stack_State ()
respect_spaces = Parsec.skipMany1 silent_space

ignore_spaces :: Parsec String Stack_State ()
ignore_spaces = Parsec.skipMany silent_space

silent_space :: Parsec String Stack_State Char
silent_space = Parsec.char ' ' <?> ""

no_spaces :: String -> Parsec String Stack_State ()
no_spaces failmsg = Parsec.try ((Parsec.try silent_space *> Parsec.unexpected failmsg) <|> return ())

if_loosely_spaced :: Parsec String Stack_State () -> Parsec String Stack_State ()
if_loosely_spaced action = do
    Tight spaced <- get_tightness
    when (not spaced) action

if_tightly_spaced :: Parsec String Stack_State () -> Parsec String Stack_State ()
if_tightly_spaced action = do
    Tight spaced <- get_tightness
    when spaced action

-- term parsers
parse_term_token :: Parsec String Stack_State TermToken
parse_term_token = parse_term_tok <|> parse_left_paren <|> parse_prefix_op

parse_term_tok :: Parsec String Stack_State TermToken
parse_term_tok = TermTok <$> (parse_num <|> parse_char <|> parse_string <|> parse_var)


parse_prefix_op :: Parsec String Stack_State TermToken
parse_prefix_op = do
    oper <- parse_oper_symbol
    spacing <- Parsec.optionMaybe respect_spaces
    case spacing of
        Nothing -> return (TightPreOp oper)
        Just () -> return (SpacedPreOp oper)
    where parse_oper_symbol = (
            Parsec.char '!' *> return Deref   <|>
            Parsec.char '@' *> return GetAddr <|>
            Parsec.char '~' *> return Negate  <|>
            Parsec.char '$' *> return ToString
            ) <?> "prefix operator"

parse_num :: Parsec String Stack_State Term
parse_num = Lit <$> read <$> Parsec.many1 Parsec.digit

parse_var :: Parsec String Stack_State Term
parse_var = Var <$> Parsec.many1 (Parsec.lower <|> Parsec.char '_')

parse_char :: Parsec String Stack_State Term
parse_char = CharLit <$> ((Parsec.char '\'') *> Parsec.anyChar <* (Parsec.char '\''))

parse_string :: Parsec String Stack_State Term
parse_string = StringLit <$> ((Parsec.char '\"') *> Parsec.many (Parsec.noneOf "\"") <* (Parsec.char '\"'))

parse_left_paren :: Parsec String Stack_State TermToken
parse_left_paren = do
    Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.char '(' *> return ()))
    ignore_spaces *> Parsec.char '(' *> return LParen

-- oper parsers
parse_oper_token :: Parsec String Stack_State OperToken
parse_oper_token =
    (check_for_oper *> apply_tight_prefix_opers *> parse_infix_oper) <|> parse_right_paren
    <?> "infix operator"

check_for_oper :: Parsec String Stack_State ()
check_for_oper = Parsec.lookAhead (Parsec.try (ignore_spaces *> Parsec.oneOf valid_op_chars)) *> return ()
    where valid_op_chars = "+-*/%^<>"


apply_tight_prefix_opers :: Parsec String Stack_State ()
apply_tight_prefix_opers = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        (tok:toks) -> case tok of
            StackTightPreOp op -> do
                make_twig op toks
                apply_tight_prefix_opers
            _ -> return ()


parse_infix_oper :: Parsec String Stack_State OperToken
parse_infix_oper = do
    spacing <- Parsec.optionMaybe respect_spaces
    case spacing of
        Nothing -> do
            if_loosely_spaced (oper_stack_push StackSpace)
            set_spacing_tight True
        Just _  -> do
            if_tightly_spaced find_left_space
    oper <- parse_oper_symbol
    if_loosely_spaced (respect_spaces <?> ("space after `" ++ show oper ++ "`"))
    if_tightly_spaced $ no_spaces ("whitespace after `" ++ show oper ++ "`")
    return (Oper oper)
    where parse_oper_symbol = (
            Parsec.char '+' *> return Plus   <|>
            Parsec.char '-' *> return Minus  <|>
            Parsec.char '*' *> return Splat  <|>
            Parsec.char '/' *> return Divide <|>
            Parsec.char '%' *> return Modulo <|>
            Parsec.char '^' *> return Hihat  <|>
            Parsec.string "<>" *> return Combine
            ) <?> "infix operator"

parse_right_paren :: Parsec String Stack_State OperToken
parse_right_paren = do
    spacing <- Parsec.optionMaybe respect_spaces
    _ <- Parsec.char ')'
    return $ case spacing of
        Nothing -> RParen
        Just () -> RParenAfterSpace

-- functions that make sure the tree is in the right order
apply_higher_prec_ops :: Precedence -> Parsec String Stack_State ()
apply_higher_prec_ops current = do
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        (tok:toks) -> case tok of
            StackSpace -> return ()
            StackLParen -> return ()
            StackLParenFollowedBySpace -> return ()
            StackTightPreOp _ -> error "i believe this should be unreachable."
            StackSpacedPreOp op -> do
                make_twig op toks
                apply_higher_prec_ops current
            StackOp op -> case (get_prec op `compare` current) of
                LT -> return ()
                _ -> do
                    make_branch op toks
                    apply_higher_prec_ops current


look_for :: StackOp -> Parsec String Stack_State ()
look_for thing = do
-- pop stuff off the oper_stack until you find `thing`
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> Parsec.unexpected "right paren"
        (tok:toks) -> case tok of
            t | t == thing -> oper_stack_set toks
            StackTightPreOp op -> do
                make_twig op toks
                look_for thing
            StackSpacedPreOp _ -> error "i believe this should be unreachable."
            StackOp op -> do
                make_branch op toks
                look_for thing
            StackLParen -> Parsec.parserFail "incorrectly spaced parentheses"
            StackLParenFollowedBySpace -> Parsec.parserFail "incorrectly spaced parentheses"
            StackSpace -> Parsec.parserFail "incorrect spacing or parentheses"


find_left_space :: Parsec String Stack_State ()
find_left_space = look_for StackSpace *> set_spacing_tight False

-- for finishing up at the end
clean_stack :: Parsec String Stack_State ()
clean_stack = do
    if_tightly_spaced find_left_space
    Oper_Stack op_stack <- get_op_stack
    case op_stack of
        [] -> return ()
        StackTightPreOp op : tokes -> do
            make_twig op tokes
            clean_stack
        StackSpacedPreOp op : tokes -> do
            make_twig op tokes
            clean_stack
        (StackOp op:tokes) -> do
            make_branch op tokes
            clean_stack
        _ -> Parsec.parserFail "incorrect whitespace or parens?"


finish_expr :: Parsec String Stack_State ASTree
finish_expr = do
    ignore_spaces
    Parsec.optional Parsec.newline <?> ""
    Parsec.eof <?> ""
    clean_stack
    Tree_Stack tree <- get_tree_stack
    case tree of
        [] -> Parsec.parserFail "bad expression"
        (result:[]) -> return result
        _ -> Parsec.parserFail "invalid expression, something is wrong here."

-- parse_term and parse_oper are alternated until one fails and finish_expr succeeds
parse_term :: Parsec String Stack_State ASTree
parse_term = do
    -- shunting yard, returns a parse tree
    toke <- parse_term_token
    case toke of
        LParen -> do
            if_tightly_spaced (oper_stack_push StackSpace *> set_spacing_tight False)
            spacing <- Parsec.optionMaybe respect_spaces
            case spacing of
                Nothing -> oper_stack_push StackLParen
                Just () -> oper_stack_push StackLParenFollowedBySpace
            parse_term
        TermTok t -> do
            tree_stack_push (Leaf t)
            parse_oper <|> finish_expr
        TightPreOp op -> do
            oper_stack_push (StackTightPreOp op)
            parse_term
        SpacedPreOp op -> do
            oper_stack_push (StackSpacedPreOp op)
            parse_term

parse_oper :: Parsec String Stack_State ASTree
parse_oper = do
    toke <- parse_oper_token
    case toke of
        RParen -> do
            if_tightly_spaced find_left_space
            look_for StackLParen
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> oper_stack_set ops *> set_spacing_tight True
                _ -> return ()
            parse_oper <|> finish_expr
        RParenAfterSpace -> do
            if_tightly_spaced find_left_space
            look_for StackLParenFollowedBySpace
            Oper_Stack stack_ops <- get_op_stack
            case stack_ops of
                (StackSpace:ops) -> oper_stack_set ops *> set_spacing_tight True
                _ -> return ()
            parse_oper <|> finish_expr
        Oper op -> do
            apply_higher_prec_ops (get_prec op)
            oper_stack_push (StackOp op)
            parse_term

parse_expression :: Parsec String Stack_State ASTree
parse_expression = parse_term

-- these are little utilities, unrelated to parsing
pretty_show :: ASTree -> String
pretty_show (Branch oper left right) = "(" ++ show oper ++ " "  ++ pretty_show left ++ " " ++ pretty_show right ++ ")"
pretty_show (Twig oper tree) = concat ["(", show oper, " ", pretty_show tree, ")"]
pretty_show (Leaf val) = show val

run_shunting_yard :: String -> Either Parsec.ParseError ASTree
run_shunting_yard input = Parsec.runParser parse_expression start_state "input" (trim_spaces input)
    where
        start_state = (Oper_Stack [], Tree_Stack [], Tight False)
        trim_spaces = dropWhile isSpace <&> dropWhileEnd isSpace

print_shunting_yard :: String -> IO ()
print_shunting_yard input = case run_shunting_yard input of
        Left err -> putStrLn (show err)
        Right tree -> putStrLn (pretty_show tree)

evaluate :: ASTree -> Integer
evaluate (Leaf t) = case t of
    Lit x -> x
    CharLit c -> fromIntegral (ord c)
    StringLit s -> fromIntegral (length s)
    Var _ -> undefined -- no way to evaluate these
evaluate (Twig op tree) = operate (evaluate tree)
    where operate = case op of
            Deref -> (\n -> product [1..n]) -- factorial, just for testing
            GetAddr -> undefined
            Negate -> negate
            ToString -> undefined
evaluate (Branch op left right) = evaluate left `operate` evaluate right
    where operate = case op of
            Plus   -> (+)
            Minus  -> (-)
            Splat  -> (*)
            Divide -> div
            Modulo -> mod
            Hihat  -> (^)
            Combine  -> undefined

eval_show :: ASTree -> String
eval_show = evaluate <&> show

parse_eval_print :: String -> IO ()
parse_eval_print input = case run_shunting_yard input of
    Left err -> putStrLn (show err)
    Right tree -> putStrLn (eval_show tree)
