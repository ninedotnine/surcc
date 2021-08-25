module TypeChecker.Context (
    ContextClass(..),
    ExportList(..),
    LocalScope(..),
    BoundLocal(..),
    insert_global,
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
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except

import Data.Text (Text)
import qualified Data.Text as Text

import Common
import Builtins (typeof_builtin_identifier)

import Parser.Expr.ExprTypes


data BoundLocal = BoundLocal Identifier SoucType Mutability deriving Eq

class ContextClass c where
    lookup :: c -> Identifier -> Maybe SoucType

data ExportList = ExportList [Bound]
data LocalScope = GlobalScope [Bound] ExportList
                | InnerScope [BoundLocal] LocalScope

-- instance ContextClass BuiltinsCtx where
-- --     lookup (Builtins bounds) ident = lookup_b bounds ident
--     lookup (Builtins bounds) ident = typeof_builtin_identifier ident

instance ContextClass ExportList where
    lookup (ExportList bounds) ident = case lookup_b bounds ident of
        Nothing -> typeof_builtin_identifier ident
        just_type -> just_type

instance ContextClass LocalScope where
    lookup (GlobalScope bounds ctx) ident = case lookup_b bounds ident of
        Nothing -> lookup ctx ident
        just_type -> just_type
    lookup (InnerScope bounds ctx) ident = case lookup_with_mut bounds ident of
        Nothing -> lookup ctx ident
        just_type -> just_type


lookup_b :: [Bound] -> Identifier -> Maybe SoucType
lookup_b [] _ = Nothing
lookup_b (b:bs) ident = case this_one b ident of
    Nothing -> lookup_b bs ident
    just_type -> just_type
    where
        this_one :: Bound -> Identifier -> Maybe SoucType
        this_one (Bound i0 t) i1 = if i0 == i1 then Just t else Nothing

lookup_with_mut :: [BoundLocal] -> Identifier -> Maybe SoucType
lookup_with_mut [] _ = Nothing
lookup_with_mut (b:rest) ident = case this_one b ident of
    Nothing -> lookup_with_mut rest ident
    just_type -> just_type
    where
        this_one :: BoundLocal -> Identifier -> Maybe SoucType
        this_one (BoundLocal i0 t _) i1 = if i0 == i1 then Just t else Nothing

lookup_mutable :: LocalScope -> Identifier -> Maybe (SoucType, Mutability)
lookup_mutable (GlobalScope bounds ctx) ident = case lookup_b bounds ident of
    Nothing -> case lookup ctx ident of
        Nothing -> Nothing
        Just t -> Just (t, Immut)
    Just t -> Just (t, Immut)
lookup_mutable (InnerScope bounds ctx) ident = case lookup_with_mut_2 bounds ident of
    Nothing -> lookup_mutable ctx ident
    just_type -> just_type
    where
        lookup_with_mut_2 [] _ = Nothing
        lookup_with_mut_2 (b:rest) i = case this_one b i of
            Nothing -> lookup_with_mut_2 rest i
            just_type -> just_type
            where
                this_one :: BoundLocal -> Identifier -> Maybe (SoucType, Mutability)
                this_one (BoundLocal i0 t m) i1 = if i0 == i1 then Just (t, m) else Nothing


add_bind :: LocalScope -> Mutability -> Bound -> Either TypeError LocalScope
add_bind ctx modifiable (Bound i t) = case lookup_mutable ctx i of
    Just (existing_type, existing_mut) -> if (modifiable, existing_mut) == (Mut, Mut)
        then if t == existing_type
            then Right ctx
            else Left (TypeMismatch existing_type t)
        else
            Left (MultipleDeclarations i)
    Nothing -> Right $ case ctx of
        GlobalScope binds rest -> GlobalScope (Bound i t : binds) rest
        InnerScope binds rest ->
            InnerScope ((BoundLocal i t modifiable) : binds) rest

define_export :: LocalScope -> Bound -> Either TypeError LocalScope
define_export scope b = case scope of
    InnerScope _ _ -> error "should not define exports from inner scope"
    GlobalScope binds ctx -> case b of
        Bound i _ -> case lookup_b binds i of
            Nothing -> Right (GlobalScope (b : binds) ctx)
            Just _ -> Left (MultipleDeclarations i)

remove_export_wrapper :: LocalScope -> Bound -> Either TypeError LocalScope
remove_export_wrapper scope  b = case scope of
    InnerScope _ _ -> error "should not remove exports from inner scope"
    GlobalScope bounds ctx -> case remove_export ctx b of
        Left err -> Left err
        Right ok_ctx -> Right (GlobalScope bounds ok_ctx)

remove_export :: ExportList -> Bound -> Either TypeError ExportList
remove_export (ExportList bounds) (Bound i t) = case lookup_b bounds i of
    Just b_t | t == b_t -> Right (ExportList (filter (\(Bound b_i _) -> b_i /= i) bounds))
    Just b_t -> Left (TypeMismatch b_t t)
    Nothing -> Left (Undeclared i)


add_potential_export :: Bound -> Checker ()
add_potential_export bound = do
    ctx <- get
    case remove_export_wrapper ctx bound of
        Left (Undeclared _) -> insert_global bound
        Left err -> throwE err
        Right removed_ctx -> case define_export removed_ctx bound of
            Left err -> throwE err
            Right new_ctx -> put new_ctx


exports_remaining :: LocalScope -> [Bound]
exports_remaining (InnerScope _ ctx) = exports_remaining ctx
exports_remaining (GlobalScope _ (ExportList [])) = []
exports_remaining (GlobalScope _ (ExportList bs)) = bs

type Checker a = ExceptT TypeError (State LocalScope) a

insert_param :: Identifier -> SoucType -> Checker ()
insert_param i t = insert_local Immut (Bound i t)

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

insert_global :: Bound -> Checker ()
insert_global bound = do
    ctx <- get
    case (add_bind ctx Immut bound) of
        Left err -> throwE err
        Right new_ctx -> put new_ctx

insert_local :: Mutability -> Bound -> Checker ()
insert_local modifiable bound = do
    ctx <- get
    case (add_bind ctx modifiable bound) of
        Left err -> throwE err
        Right new_ctx -> put new_ctx

insert_mut :: Bound -> Checker ()
insert_mut = insert_local Mut

insert_const :: Bound -> Checker ()
insert_const = insert_local Immut
