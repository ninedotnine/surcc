module SurCC.TypeChecker.Expressions (
    infer,
    check_expr,
    infer_if_needed,
) where

import Control.Applicative
import Control.Monad (unless)
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.Functor ((<&>))

import SurCC.Common
import SurCC.TypeChecker.Context (
    lookup_identifier,
    new_scope,
    new_pattern_scope,
    exit_scope,
    insert_immut,
    LocalScope,
    Checker,
    )

infer :: ExprTree -> Checker SoucType
infer = \case
    Branch op left right -> infer_infix_op left right op
    Twig op expr -> infer_prefix_op expr op
    Signed expr t -> do
        check_expr t expr
        pure t
    Leaf term -> infer_term term
    Match scrutinee branches -> do
        pat_t <- infer scrutinee
        case branches of
            -- the expression parser should require at least one branch
            -- FIXME consider a non-empty list type
            [] -> error "unreachable: empty cases in a match expression"
            ((pat,expr):etc) -> do
                -- the pattern might bind an identifier,
                -- and the expr must be checked in that context
                new_scope
                check_pattern pat_t pat
                expr_t <- infer expr
                exit_scope
                check_branches (pat_t,expr_t) etc
                pure expr_t


infer_term :: Term -> Checker SoucType
infer_term = \case
    Lit l -> infer_lit l
    Var v -> get >>= \ctx -> case lookup_identifier v ctx of
        -- FIXME is this unreachable?
        -- the scrutinee has already been inferred
        Nothing -> throwE (Undeclared v)
        Just t -> pure t
    Constructor s -> case s of
        "True" -> pure SoucBool
        "False" -> pure SoucBool
        "None" -> pure (SoucMaybe SoucInteger)
        "OK" -> pure (SoucMaybe (SoucFn SoucInteger SoucInteger))
        _ -> throwE (UnknownData s)


infer_lit :: Literal -> Checker SoucType
infer_lit = pure <$> \case
    LitInt _    -> SoucInteger
    LitChar _   -> SoucChar
    LitString _ -> SoucString


infer_prefix_op :: ExprTree -> PrefixOperator -> Checker SoucType
infer_prefix_op expr = \case
    Deref -> not_implemented
    GetAddr -> not_implemented
    Negate -> do
        check_expr SoucBool expr
        pure SoucBool
    ToString -> do
        check_expr SoucInteger expr
        pure SoucString
    Pure -> not_implemented
    Join -> not_implemented


infer_infix_op :: ExprTree -> ExprTree -> Operator -> Checker SoucType
infer_infix_op left right = \case
    -- FIXME we're not polymorphic yet
    Plus  -> do
        check_expr SoucInteger left
        check_expr SoucInteger right
        pure SoucInteger
    Minus -> infer_infix_op left right Plus
    Splat -> infer_infix_op left right Plus

    And -> do
        check_expr SoucBool left
        check_expr SoucBool right
        pure SoucBool
    Or  -> infer_infix_op left right And
    Xor -> infer_infix_op left right And

    Equals -> do
        check_expr SoucInteger left
        check_expr SoucInteger right
        pure SoucBool
    LesserThan -> infer_infix_op left right Equals
    GreaterThan -> infer_infix_op left right Equals

    Apply -> do
        l_t <- infer left
        case l_t of
            SoucFn t0 t1 -> do
                check_expr t0 right
                pure t1
            _ -> throwE (TypeMismatch (SoucFn l_t (SoucTypeVar (TypeVar (Right 'T') (SoucKind 0)))) l_t)
    FlipApply -> infer_infix_op right left Apply

    Comma -> do
        l_t <- infer left
        r_t <- infer right
        pure (SoucPair l_t r_t)

    _ -> not_implemented


check_expr :: SoucType -> ExprTree -> Checker ()
check_expr t expr = infer expr >>= assert_equals t


check_branches :: (SoucType,SoucType) -> [(Pattern,ExprTree)] -> Checker ()
check_branches (pat_t,expr_t) = foldr ((*>) . check_branch) (pure ())
    where
        check_branch :: (Pattern,ExprTree) -> Checker ()
        check_branch (pat,expr) = do
            new_scope
            check_pattern pat_t pat
            check_expr expr_t expr
            exit_scope


check_lit :: SoucType -> Literal -> Checker ()
check_lit t l = do
    l_t <- infer_lit l
    assert_equals t l_t


check_pattern :: SoucType -> Pattern -> Checker ()
check_pattern t = \case
    PatLit l -> check_lit t l
    PatBinding i -> insert_immut i t


infer_if_needed :: Maybe SoucType -> ExprTree -> Checker SoucType
infer_if_needed m_t expr = do
    case m_t of
        Nothing -> infer expr
        Just t -> check_expr t expr *> pure t


assert_equals :: SoucType -> SoucType -> Checker ()
assert_equals t0 t1 = unless (t0 == t1) (throwE (TypeMismatch t0 t1))


not_implemented :: Checker a
not_implemented = throwE $ TypeMismatch (SoucType "NOT YET" (SoucKind 0)) (SoucType "IMPLEMENTED" (SoucKind 0))
