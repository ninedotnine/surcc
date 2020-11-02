module TypeChecker.Context (
    ContextClass(..),
    Builtins(..),
    ExportList(..),
    LocalScope(..),
    BoundLocal(..),
    Mutability(..),
    add_bind,
    insert_global,
    insert_param,
    BindMayExist(..),
    builtins_ctx,
    Checker,
    insert,
    add_potential_export,
    exports_remaining,
) where

import Prelude hiding (lookup)
import Control.Monad.State

import Common
import Parser.Expr.ExprTypes

-- FIXME rename this ReAssignable or something
newtype BindMayExist = BindMayExist Bool

data BoundLocal = BoundLocal Identifier SoucType Mutability deriving Eq

instance Show BoundLocal where
    show (BoundLocal (Identifier i) t Mut) = "Bound (mutable)" ++ i ++ ": " ++ show t
    show (BoundLocal (Identifier i) t Immut) = "Bound " ++ i ++ ": " ++ show t

data Mutability = Mut | Immut deriving (Show, Eq)


class ContextClass c where
    lookup :: c -> Identifier -> Maybe SoucType

data Builtins = Builtins [Bound] deriving Show
data ExportList = ExportList [Bound] Builtins deriving Show
data LocalScope = GlobalScope [Bound] ExportList
                | InnerScope [BoundLocal] LocalScope
                deriving Show

instance ContextClass Builtins where
    lookup (Builtins bounds) ident = lookup_b bounds ident

instance ContextClass ExportList where
    lookup (ExportList bounds ctx) ident = case lookup_b bounds ident of
        Nothing -> lookup ctx ident
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


add_bind :: LocalScope -> BindMayExist -> Bound -> Either TypeError LocalScope
add_bind ctx (BindMayExist modifiable) (Bound i t) = case lookup_mutable ctx i of
    Just (existing_type, existing_mut) -> if modifiable && existing_mut == Mut
        then if t == existing_type
            then Right ctx
            else Left (TypeMismatch existing_type t)
        else
            Left (MultipleDeclarations i)
    Nothing -> Right $ case ctx of
        GlobalScope binds rest -> GlobalScope (Bound i t : binds) rest
        InnerScope binds rest ->
            InnerScope ((BoundLocal i t (if modifiable then Mut else Immut)) : binds) rest

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
remove_export (ExportList bounds ctx) (Bound i t) = case lookup_b bounds i of
    Just b_t | t == b_t -> Right (ExportList (filter (\(Bound b_i _) -> b_i /= i) bounds) ctx)
    Just b_t -> Left (TypeMismatch b_t t)
    Nothing -> Left (Undeclared i)

add_potential_export :: Bound -> Checker ()
add_potential_export bound = do
    ctx <- get
    case remove_export_wrapper ctx bound of
        Left (Undeclared _) -> insert (BindMayExist False) bound
        Left err -> pure (Left err)
        Right removed_ctx -> case define_export removed_ctx bound of
            Left err -> pure (Left err)
            Right new_ctx -> put new_ctx >> pure (Right ())


exports_remaining :: LocalScope -> [Bound]
exports_remaining (InnerScope _ ctx) = exports_remaining ctx
exports_remaining (GlobalScope _ (ExportList [] _)) = []
exports_remaining (GlobalScope _ (ExportList bs _)) = bs

type Checker a = State LocalScope (Either TypeError a)

insert :: BindMayExist -> Bound -> Checker ()
insert = insert_local

insert_param :: Identifier -> SoucType -> Checker ()
insert_param i t = insert_local (BindMayExist False) (Bound i t)

insert_global :: Bound -> Checker ()
insert_global bound = do
    ctx <- get
    case (add_bind ctx (BindMayExist False) bound) of
        Left err -> pure (Left err)
        Right new_ctx -> put new_ctx >> pure (Right ())

insert_local :: BindMayExist -> Bound -> Checker ()
insert_local modifiable bound = do
    ctx <- get
    case (add_bind ctx modifiable bound) of
        Left err -> pure (Left err)
        Right new_ctx -> put new_ctx >> pure (Right ())


builtins_ctx :: Builtins
builtins_ctx = Builtins [Bound "puts" (SoucRoutn (SoucType "String"))]
