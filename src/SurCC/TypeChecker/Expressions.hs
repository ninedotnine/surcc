module SurCC.TypeChecker.Expressions (
    infer,
    check_expr,
    infer_if_needed,
) where

import Control.Applicative
import Control.Monad.Trans.Except
import Control.Monad.State
import Data.Functor ((<&>))

import SurCC.Common
import SurCC.TypeChecker.Context (
    lookup_identifier,
    new_pattern_scope,
    exit_scope,
    LocalScope,
    Checker,
    )
import SurCC.TypeChecker.Operators

infer :: ExprTree -> Checker SoucType
infer = \case
    Branch op left right -> ret <$> infer_infix_op op left right
    Twig op expr -> ret <$> infer_prefix_op op expr
    Signed expr t -> do
        inferred <- infer expr
        check_equals t inferred
        pure inferred
    Leaf term -> infer_term term
    Match expr branches -> do
        expr_t <- infer expr
        check_patterns expr_t (branches <&> fst)
        infer_cases (branches <&> snd)
        where
            infer_cases :: [ExprTree] -> Checker SoucType
            infer_cases = \case
                -- the expression parser should require at least one branch
                [] -> error "unreachable: empty cases in a match expression"
                (c:cs) -> do
                    t <- infer c
                    check_cases t cs
                    pure t



infer_term :: Term -> Checker SoucType
infer_term term = case term of
    Lit l -> case l of
        LitInt _    -> pure SoucInteger
        LitChar _   -> pure SoucChar
        LitString _ -> pure SoucString
    Var v -> get >>= \ctx -> case lookup_identifier v ctx of
        Nothing -> throwE (Undeclared v)
        Just t -> pure t
    Constructor s -> case s of
        "True" -> pure SoucBool
        "False" -> pure SoucBool
        "None" -> pure (SoucMaybe SoucInteger)
        "OK" -> pure (SoucMaybe (SoucFn SoucInteger SoucInteger))
        _ -> throwE (UnknownData s)


not_implemented :: Checker a
not_implemented = throwE $ TypeMismatch (SoucType "NOT YET" (SoucKind 0)) (SoucType "IMPLEMENTED" (SoucKind 0))

infer_prefix_op :: PrefixOperator -> ExprTree -> Checker (InputType, ReturnType)
infer_prefix_op op _ = case op of
    Deref -> not_implemented
    GetAddr -> not_implemented
    Negate -> pure (in_t "Bool", ret_t "Bool")
    ToString -> pure (in_t "Integer", ret_t "String")
    Pure -> not_implemented
    Join -> not_implemented


infer_infix_op :: Operator -> ExprTree -> ExprTree
                  -> Checker ((InputType, InputType), ReturnType)
infer_infix_op op left right = case op of
    Plus  -> pure ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    Minus -> pure ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    Splat -> pure ((in_t "Integer", in_t "Integer"), ret_t "Integer")
    And -> pure ((in_t "Bool", in_t "Bool"), ret_t "Bool")
    Or  -> pure ((in_t "Bool", in_t "Bool"), ret_t "Bool")
    Equals -> pure ((in_t "Integer", in_t "Integer"), ret_t "Bool") -- FIXME (should be general)
    LesserThan -> pure ((in_t "Integer", in_t "Integer"), ret_t "Bool") -- FIXME (should be general)
    GreaterThan -> pure ((in_t "Integer", in_t "Integer"), ret_t "Bool") -- FIXME (should be general)
    Apply     -> do
        l_t <- infer left
        case l_t of
            SoucFn t0 t1 -> do
                r_t <- infer right
                check_equals r_t t0
                pure ((InputType l_t, InputType t0), ReturnType t1)
            -- FIXME  l_t is used twice here and r_t is not used?
            _ -> throwE (TypeMismatch (SoucFn l_t (SoucTypeVar (TypeVar (Right 'T') (SoucKind 0)))) l_t)
    FlipApply -> do
        r_t <- infer right
        l_t <- infer left
        case r_t of
            SoucFn t0 t1 -> do
                check_equals l_t t0
                pure (((InputType t0, InputType r_t)), ReturnType t1)
            _ -> throwE (TypeMismatch (SoucFn l_t (SoucTypeVar (TypeVar (Right 'T') (SoucKind 0)))) r_t)
    Comma -> do
        l_t <- infer left
        r_t <- infer right
        pure ((InputType l_t, InputType r_t), ReturnType (SoucPair l_t r_t))

    _ -> not_implemented




check_equals :: SoucType -> SoucType -> Checker ()
check_equals t0 t1 = if t0 == t1 then pure () else throwE (TypeMismatch t0 t1)


check_expr :: SoucType -> ExprTree -> Checker ()
check_expr t = \case
    Branch op left right -> do
        ((InputType l_t, InputType r_t), ReturnType expr_t) <- infer_infix_op op left right
        check_expr l_t left
        check_expr r_t right
        check_equals t expr_t


    Twig op expr -> do
        (InputType arg_t, ReturnType expr_t) <- infer_prefix_op op expr
        check_expr arg_t expr
        check_equals t expr_t

    Leaf term -> do
        term_t <- infer_term term
        check_equals t term_t

    Signed expr sig -> do
        expr_t <- infer expr
        check_expr expr_t expr
        check_equals sig expr_t
        check_equals t expr_t

    Match expr branches -> do
        expr_t <- infer expr
        check_patterns expr_t (branches <&> fst)
        check_cases t (branches <&> snd)


check_patterns :: SoucType -> [Pattern] -> Checker ()
check_patterns t = foldr ((>>) . (check_pattern t)) (pure ())


check_pattern :: SoucType -> Pattern
                 -> Checker ()
check_pattern t = \case
    PatLit l -> case l of
        LitInt _ -> check_equals t SoucInteger
        LitChar _ -> check_equals t SoucChar
        LitString _ -> check_equals t SoucString
    PatId i -> get >>= \ctx -> case lookup_identifier i ctx of
        Nothing -> do
            -- FIXME bind this identifier in a new context?
            pure ()
        Just i_t -> check_equals t i_t
    PatBinding i -> undefined i


check_pattern_new :: SoucType -> Pattern
                 -> Checker [(Identifier, SoucType)]
check_pattern_new t = \case
    PatLit l -> case l of
        LitInt _ -> check_equals t SoucInteger *> pure []
        LitChar _ -> check_equals t SoucChar *> pure []
        LitString _ -> check_equals t SoucString *> pure []
    PatId i -> get >>= \ctx -> case lookup_identifier i ctx of
        Nothing -> do
            -- FIXME bind this identifier in a new context?
            pure []
        Just i_t -> check_equals t i_t *> pure []
    PatBinding i -> undefined i



check_cases :: SoucType -> [ExprTree] -> Checker ()
check_cases t = foldr ((>>) . check_expr t) (pure ())


check_pats_cases :: SoucType -> [(Pattern,ExprTree)] -> Checker ()
-- check_pats_cases ctx t = foldr ((>>) . check_expr ctx t) (pure ())
check_pats_cases t = undefined t -- FIXME


check_pat_case :: SoucType -> (Pattern,ExprTree) -> Checker ()
check_pat_case t (pat,expr) = do
    binds <- check_pattern_new t pat
    new_pattern_scope binds
    undefined expr binds -- FIXME


infer_if_needed :: Maybe SoucType -> ExprTree -> Checker SoucType
infer_if_needed m_t expr = do
    case m_t of
        Nothing -> infer expr
        Just t -> check_expr t expr *> pure t
