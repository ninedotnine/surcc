{-# LANGUAGE FlexibleContexts #-}

module SurCC.TypeChecker.Expressions (
    infer,
    check_expr,
    checkm_expr,
    assert_equals,
) where

import Prelude hiding (lookup)

import Control.Applicative ()
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Trans.Except
import Data.Functor ((<&>))

import SurCC.Common
import SurCC.Common.SoucTypes

import SurCC.TypeChecker.Context (
    get_type,
    new_scope,
    exit_scope,
    insert_local,
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
            ((pat,g,expr):etc) -> do
                -- the pattern might bind an identifier,
                -- and the expr must be checked in that context
                new_scope
                check_pattern pat_t pat
                check_guard g
                expr_t <- infer expr
                exit_scope
                check_branches (pat_t,expr_t) etc
                pure expr_t


infer_term :: Term -> Checker SoucType
infer_term = \case
    Lit l -> infer_lit l
    Name n -> get_type n


infer_lit :: MonadError TypeError m => Literal -> m SoucType
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
            _ -> throwError $
                    TypeMismatch (
                        SoucFn l_t (
                            SoucTypeVar (TypeVar (Right 'T')
                            (SoucKind 0))))
                        l_t
    FlipApply -> infer_infix_op right left Apply

    Comma -> do
        l_t <- infer left
        r_t <- infer right
        pure (SoucPair l_t r_t)

    _ -> not_implemented


check_expr :: SoucType -> ExprTree -> Checker ()
check_expr t expr = infer expr >>= assert_equals t


check_branches :: (SoucType,SoucType) -> [(Pattern,Maybe Guard,ExprTree)]
                  -> Checker ()
check_branches (pat_t,expr_t) = foldr ((*>) . check_branch) (pure ())
    where
        check_branch :: (Pattern,Maybe Guard,ExprTree) -> Checker ()
        check_branch (pat,g,expr) = do
            new_scope
            check_pattern pat_t pat
            check_guard g
            check_expr expr_t expr
            exit_scope


check_guard :: Maybe Guard -> Checker ()
check_guard = \case
    Nothing -> pure ()
    Just (Guard g) -> check_expr SoucBool g

check_lit :: MonadError TypeError m => SoucType -> Literal -> m ()
check_lit t l = do
    l_t <- infer_lit l
    assert_equals t l_t


check_pattern :: SoucType -> Pattern -> Checker ()
check_pattern t = \case
    PatLit l -> check_lit t l
    PatConst i -> assert_equals t =<< get_type i
    PatBinding i -> insert_local Immut i t


checkm_expr :: Maybe SoucType -> ExprTree -> Checker SoucType
checkm_expr m_t expr = case m_t of
    Nothing -> infer expr
    Just t -> check_expr t expr *> pure t


assert_equals :: (MonadError TypeError m) => SoucType -> SoucType -> m ()
assert_equals t0 t1 = unless (t0 == t1) (throwError (TypeMismatch t0 t1))


not_implemented :: MonadError TypeError m => m a
not_implemented = throwError $
    TypeMismatch (SoucType "NOT YET") (SoucType "IMPLEMENTED")
