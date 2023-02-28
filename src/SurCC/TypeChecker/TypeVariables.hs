module SurCC.TypeChecker.TypeVariables (
    ) where

import Control.Applicative
import Control.Monad (unless)
-- import Data.HashMap.Strict qualified as Map
import Data.Function
import Data.Functor
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Prelude hiding (lookup)
import SurCC.Common
import SurCC.Common.SoucTypes


souc_kind :: SoucType -> SoucKind
souc_kind = \case
    SoucTypeCon _ args -> args & List.genericLength & SoucKind
    SoucTypeVar (TypeVar _ k) -> k

souc_var_kind :: TypeVar -> SoucKind
souc_var_kind (TypeVar _ k) = k

-- get a list of type variables that occur in a type.
-- the list might contain duplicates.
tvars :: SoucType -> [TypeVar]
tvars t = go t []
    where
        go :: SoucType -> [TypeVar] -> [TypeVar]
        go tv list = case tv of
            SoucTypeVar v -> v : list
            SoucTypeCon _ etc -> foldr go list etc

class Types t where
    apply :: Subst -> t -> t
    tvs :: t -> [TypeVar]

instance Types SoucType where
    apply s = \case
        SoucType t -> SoucType t
        SoucTypeVar v -> apply_subst v s
        SoucTypeCon name ts -> SoucTypeCon name (apply s <$> ts)

    -- FIXME test this
    tvs = \case
        SoucType _ -> []
        SoucTypeVar v -> [v]
        SoucTypeCon _ ts -> ts >>= tvs

instance Types a => Types [a] where
    apply s = fmap (apply s)
    tvs = tvs & concatMap <&> List.nub


type Subst = Map.Map TypeVar SoucType


substitute_type :: Subst -> SoucType -> SoucType
substitute_type s = \case
    SoucTypeVar v -> apply_subst v s
    SoucTypeCon t1 ts -> SoucTypeCon t1 (ts <&> substitute_type s)


apply_subst :: TypeVar -> Subst -> SoucType
apply_subst v dict = Map.lookup v dict // SoucTypeVar v


null_subst :: Subst
null_subst = mempty


delta_subst :: TypeVar -> SoucType -> Subst
delta_subst = Map.singleton


-- FIXME definitely test this
subst_compose :: Subst -> Subst -> Subst
-- subst_compose s1 s0 = Map.unionWith (substitute_type s1) s0 s1
-- subst_compose s1 s0 = fmap (substitute_type s1) s0
subst_compose s1 s0 = Map.union (fmap (substitute_type s1) s0) s1


-- FIXME test this
subst_merge :: MonadFail m => Subst -> Subst -> m Subst
subst_merge s0 s1 = if agree
    then pure (Map.union s0 s1)
    else fail "merge failed"
        where
            agree :: Bool
            agree = and shared
            shared :: Map.Map TypeVar Bool
            shared = (Map.intersectionWith (==) s0 s1)


-- most general unifier
mgu :: MonadFail m => (SoucType,SoucType) -> m Subst
mgu = \case
    (SoucType t0,SoucType t1)
        | t0 == t1 -> pure null_subst
    (SoucTypeVar v,t) -> var_bind (v,t)
    (t,SoucTypeVar v) -> var_bind (v,t)
    (SoucTypeCon name0 [arg0],SoucTypeCon name1 [arg1]) -> do
        unless (name0 == name1) $
            fail "names not equal"
        mgu (arg0,arg1)
--     (SoucTypeCon name0 ts0,SoucTypeCon name1 ts1) -> do
--         unless (name0 == name1) $
--             fail "names not equal"
--         foldr subst_compose null_subst mgu
    _ -> fail "types do not unify"

var_bind :: MonadFail m => (TypeVar,SoucType) -> m Subst
var_bind = \case
    (v,t)
        | t == SoucTypeVar v -> pure null_subst
        | v `elem` tvars t -> fail "occurs check failed"
        | souc_var_kind v /= souc_kind t -> fail "unequal kinds"
        | otherwise -> pure $ delta_subst v t

match :: MonadFail m => (SoucType,SoucType) -> m Subst
match = \case
    (SoucType t0,SoucType t1)
        | t0 == t1 -> pure null_subst
    (SoucTypeVar v,t) -> compare_kinds v t
    (t,SoucTypeVar v) -> compare_kinds v t
    (SoucTypeCon name0 [arg0],SoucTypeCon name1 [arg1]) -> do
        unless (name0 == name1) $
            fail "match: names not equal"
        match (arg0,arg1)
--         subst_merge
--     (SoucTypeCon name0 ts0,SoucTypeCon name1 ts1) -> do
--         unless (name0 == name1) $
--             fail "names not equal"
--         foldr subst_compose null_subst mgu
    _ -> fail "types do not match"
    where
        compare_kinds :: MonadFail m => TypeVar -> SoucType -> m Subst
        compare_kinds v t = if souc_var_kind v == souc_kind t
            then pure $ delta_subst v t
            else fail "kinds not equal"
