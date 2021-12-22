module TypeChecker.Context (
    ExportList(..),
    LocalScope(..),
    BoundLocal(..),
    lookup_identifier,
    lookup_constant,
    new_scope,
    new_param_scope,
    exit_scope,
    insert_const,
    insert_mut,
    Checker,
    add_potential_export,
    exports_remaining,
) where

import Prelude hiding (lookup)
import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Text (Text)
import qualified Data.Text as Text

import Common
import Builtins (typeof_builtin)



-- import qualified Data.Map.Strict as Map
-- type Mapping = Map.Map (Either Identifier Constant) SoucType
-- data ExportList = ExportList Mapping
-- data Context = GlobalScope Mapping ExportList
--              | InnerScope Map.Map Identifier (SoucType, Mutability) LocalScope
--
-- type Checker a = ExceptT TypeError (State LocalScope) a

data BoundLocal = BoundLocal Identifier SoucType Mutability deriving Eq

newtype ExportList = ExportList [Bound]

data LocalScope = GlobalScope [Bound] ExportList
                | InnerScope [BoundLocal] LocalScope

lookup_identifier :: Identifier -> LocalScope -> Maybe SoucType
lookup_identifier x ctx = lookup ctx (Left x)

lookup_constant :: Constant -> LocalScope -> Maybe SoucType
lookup_constant x ctx = lookup ctx (Right x)

lookup :: LocalScope -> Either Identifier Constant -> Maybe SoucType
lookup (GlobalScope bounds ctx) ident = case lookup_b bounds ident of
    Nothing -> lookup_exports ctx ident
    just_type -> just_type
lookup (InnerScope _ ctx) (Right constant) = lookup ctx (Right constant)
lookup (InnerScope bounds ctx) (Left ident) = case lookup_bl bounds ident of
    Nothing -> lookup ctx (Left ident)
    just_type -> just_type


lookup_exports :: ExportList -> Either Identifier Constant -> Maybe SoucType
lookup_exports (ExportList bounds) ident = case lookup_b bounds ident of
    Nothing -> typeof_builtin ident
    just_type -> just_type


lookup_b :: [Bound] -> Either Identifier Constant -> Maybe SoucType
lookup_b [] _ = Nothing
lookup_b (b:bs) ident = case this_one b ident of
    Nothing -> lookup_b bs ident
    just_type -> just_type
    where
        this_one :: Bound -> Either Identifier Constant -> Maybe SoucType
        this_one (Bound val0 t) val1 = if val0 == val1 then Just t else Nothing

lookup_bl :: [BoundLocal] -> Identifier -> Maybe SoucType
lookup_bl [] _ = Nothing
lookup_bl (b:rest) ident = case this_one b ident of
    Nothing -> lookup_bl rest ident
    just_type -> just_type
    where
        this_one :: BoundLocal -> Identifier -> Maybe SoucType
        this_one (BoundLocal i0 t _) i1 = if i0 == i1 then Just t else Nothing

lookup_mutable :: LocalScope -> Identifier -> Maybe (SoucType, Mutability)
lookup_mutable (GlobalScope bounds ctx) ident = case lookup_b bounds (Left ident) of
    Nothing -> case lookup_exports ctx (Left ident) of
        Nothing -> Nothing
        Just t -> Just (t, Immut)
    Just t -> Just (t, Immut)
lookup_mutable (InnerScope bounds ctx) ident = case lookup_bl_mutable bounds ident of
    Nothing -> lookup_mutable ctx ident
    just_type -> just_type
    where
        lookup_bl_mutable [] _ = Nothing
        lookup_bl_mutable (b:rest) i = case this_one b i of
            Nothing -> lookup_bl_mutable rest i
            just_type -> just_type
            where
                this_one :: BoundLocal -> Identifier -> Maybe (SoucType, Mutability)
                this_one (BoundLocal i0 t m) i1 = if i0 == i1 then Just (t, m) else Nothing


add_bind :: LocalScope -> Mutability -> Identifier -> SoucType
            -> Either TypeError LocalScope
add_bind ctx modifiable i t = case lookup_mutable ctx i of
    Just (existing_type, existing_mut) -> if (modifiable, existing_mut) == (Mut, Mut)
        then if t == existing_type
            then Right ctx
            else Left (TypeMismatch existing_type t)
        else
            Left (MultipleDeclarations i)
    Nothing -> Right $ case ctx of
        GlobalScope binds rest -> GlobalScope (bound_id i t : binds) rest
        InnerScope binds rest ->
            InnerScope ((BoundLocal i t modifiable) : binds) rest


define_export :: LocalScope -> Bound -> Either TypeError LocalScope
define_export scope b = case scope of
    InnerScope _ _ -> error "should not define exports from inner scope"
    GlobalScope binds ctx -> case b of
        Bound bind _ -> case lookup_b binds bind of
            Nothing -> Right (GlobalScope (b : binds) ctx)
            Just _ -> case bind of
                Left i -> Left (MultipleDeclarations i)
                Right _ -> error "define_export a constant!?"

remove_export_wrapper :: LocalScope -> Bound -> Either TypeError LocalScope
remove_export_wrapper scope  b = case scope of
    InnerScope _ _ -> error "should not remove exports from inner scope"
    GlobalScope bounds ctx -> case remove_export ctx b of
        Left err -> Left err
        Right ok_ctx -> Right (GlobalScope bounds ok_ctx)

remove_export :: ExportList -> Bound -> Either TypeError ExportList
remove_export (ExportList bounds) (Bound b t) = case lookup_b bounds b of
    Just b_t | t == b_t -> Right (ExportList (filter (\(Bound b_i _) -> b_i /= b) bounds))
    Just b_t -> Left (TypeMismatch b_t t)
    Nothing -> case b of
        Left i -> Left (Undeclared i)
        Right _ -> error "remove_export !?"


add_potential_export :: Bound -> Checker ()
add_potential_export bound@(Bound (Left i) t) = do
    ctx <- get
    case remove_export_wrapper ctx bound of
        Left (Undeclared _) -> insert_global i t
        Left err -> throwE err
        Right removed_ctx -> case define_export removed_ctx bound of
            Left err -> throwE err
            Right new_ctx -> put new_ctx
add_potential_export (Bound (Right _) _) = error "fixme haha"


exports_remaining :: LocalScope -> [Bound]
exports_remaining (InnerScope _ ctx) = exports_remaining ctx
exports_remaining (GlobalScope _ (ExportList [])) = []
exports_remaining (GlobalScope _ (ExportList bs)) = bs

type Checker a = ExceptT TypeError (State LocalScope) a

insert_param :: Identifier -> SoucType -> Checker ()
insert_param i t = insert_local Immut i t

new_scope :: Checker ()
new_scope = get >>= put . InnerScope []

new_param_scope :: Identifier -> SoucType -> Checker ()
new_param_scope i t = get >>= put . InnerScope [BoundLocal i t Immut]

exit_scope :: Checker ()
exit_scope = get >>= \case
    InnerScope _ inner -> put inner >> pure ()
    GlobalScope _ _ -> throwE (Undeclared "should be unreachable")

in_scope :: (a -> Checker Bound) -> a -> Checker ()
in_scope act x = do
    new_scope
    bound <- act x
    exit_scope
    add_potential_export bound

insert_global :: Identifier -> SoucType -> Checker ()
insert_global i t = do
    ctx <- get
    case (add_bind ctx Immut i t) of
        Left err -> throwE err
        Right new_ctx -> put new_ctx

insert_local :: Mutability -> Identifier -> SoucType -> Checker ()
insert_local modifiable i t = do
    ctx <- get
    case (add_bind ctx modifiable i t) of
        Left err -> throwE err
        Right new_ctx -> put new_ctx

insert_mut :: Identifier -> SoucType -> Checker ()
insert_mut = insert_local Mut

insert_const :: Identifier -> SoucType -> Checker ()
insert_const = insert_local Immut
