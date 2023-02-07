module SurCC.TypeChecker.TypeVariables (
    ) where

import Control.Applicative
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text

import Prelude hiding (lookup)
import SurCC.Common


type Subst = Map.Map (Either Word Char) SoucType

souc_kind :: SoucType -> SoucKind
souc_kind = \case
    SoucType _ k -> k
    SoucTypeConstructor _ k _ -> k
    SoucTypeVar (TypeVar _ k) -> k

souc_var_kind :: TypeVar -> SoucKind
souc_var_kind (TypeVar _ k) = k

substitute_type :: Subst -> SoucType -> SoucType
substitute_type subst t = case t of
    SoucTypeVar (TypeVar v _) -> Map.lookup v subst // t
    SoucTypeConstructor t1 k ts -> SoucTypeConstructor t1 k (substitute_type subst <$> ts)
    SoucType t0 k -> SoucType t0 k

class Types t where
    apply :: Subst -> t -> t
    tvs :: t -> [Either Word Char]

instance Types SoucType where
    apply s t = case t of
        SoucTypeVar (TypeVar v _) -> Map.lookup v s // t
        SoucTypeConstructor name k ts -> SoucTypeConstructor name k (apply s <$> ts)
        souc_type -> souc_type

    tvs = \case
        SoucTypeVar (TypeVar v _) -> [v]
        SoucTypeConstructor _ _ ts -> ts >>= tvs
        SoucType _ _ -> []


vars :: SoucType -> [TypeVar]
vars = undefined
