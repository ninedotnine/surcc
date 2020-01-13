module Parser.Expr.StackManipulations (
    get_op_stack,
    get_tree_stack,
    oper_stack_push,
    oper_stack_set,
    tree_stack_push,
    tree_stack_pop,
    make_branch,
    make_twig,
    look_for,
    apply_higher_prec_ops,
    find_left_space,
    clean_stack
) where

import qualified Text.Parsec as Parsec

import Parser.Expr.ExprTypes
import Parser.Expr.RegardingSpaces

-- functions to get the current state
get_op_stack :: ShuntingYardParser Oper_Stack
get_op_stack = do
    (stack, _, _) <- Parsec.getState
    return stack

get_tree_stack :: ShuntingYardParser Tree_Stack
get_tree_stack = do
    (_, stack, _) <- Parsec.getState
    return stack

-- functions to change or set the current state
oper_stack_push :: StackOp -> ShuntingYardParser ()
oper_stack_push op =
    Parsec.modifyState (\(Oper_Stack ops, terms, b) -> (Oper_Stack (op:ops), terms, b))

oper_stack_set :: [StackOp] -> ShuntingYardParser ()
oper_stack_set tokes = Parsec.modifyState (\(_,s2,b) -> (Oper_Stack tokes, s2, b))

tree_stack_push :: ASTree -> ShuntingYardParser ()
tree_stack_push tree =
    Parsec.modifyState (\(ops, Tree_Stack vals, b) -> (ops, Tree_Stack (tree:vals), b))

tree_stack_pop :: ShuntingYardParser ASTree
tree_stack_pop = do
    (opers, vals, b) <- Parsec.getState
    case vals of
        Tree_Stack (v:vs) -> do
            Parsec.setState (opers, Tree_Stack vs, b)
            return v
        Tree_Stack _ -> Parsec.unexpected "?? did i expect a term?"

-- functions that build the ASTree
make_branch :: Operator -> [StackOp] -> ShuntingYardParser ()
make_branch op tokes = do
    r <- tree_stack_pop
    l <- tree_stack_pop
    tree_stack_push (Branch op l r)
    oper_stack_set tokes

make_twig :: PrefixOperator -> [StackOp] -> ShuntingYardParser ()
make_twig op tokes = do
    tree <- tree_stack_pop
    tree_stack_push (Twig op tree)
    oper_stack_set tokes


-- functions that make sure the tree is in the right order
apply_higher_prec_ops :: Precedence -> ShuntingYardParser ()
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


look_for :: StackOp -> ShuntingYardParser ()
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
            StackSpacedPreOp op -> do
                make_twig op toks
                look_for thing
            StackOp op -> do
                make_branch op toks
                look_for thing
            StackLParen -> Parsec.parserFail "incorrectly spaced parentheses"
            StackLParenFollowedBySpace -> Parsec.parserFail "incorrectly spaced parentheses"
            StackSpace -> Parsec.parserFail "incorrect spacing or parentheses"

find_left_space :: ShuntingYardParser ()
find_left_space = look_for StackSpace *> set_spacing_tight False

-- for finishing up at the end
clean_stack :: ShuntingYardParser ()
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

