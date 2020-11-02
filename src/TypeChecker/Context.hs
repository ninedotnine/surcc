module TypeChecker.Context (
    ContextClass(..),
    Builtins(..),
    ExportList(..),
    LocalScope(..),
    Bound(..),
    lookup,
    add_bind,
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

class ContextClass c where
    lookup :: c -> Identifier -> Maybe SoucType

data Builtins = Builtins [Bound] deriving Show
data ExportList = ExportList [Bound] Builtins deriving Show
-- data GlobalScope = GlobalScope [Bound] ExportList deriving Show
data LocalScope = GlobalScope [Bound] ExportList
                | OuterScope [Bound] LocalScope
                | InnerScope [Bound] LocalScope
                deriving Show

instance ContextClass Builtins where
    lookup (Builtins bounds) ident = lookup_b bounds ident

instance ContextClass ExportList where
    lookup (ExportList bounds ctx) ident = case lookup_b bounds ident of
        Nothing -> lookup ctx ident
        just_type -> just_type

-- instance ContextClass GlobalScope where
--     lookup (GlobalScope bounds ctx) ident = case lookup_b bounds ident of
--         Nothing -> lookup ctx ident
--         just_type -> just_type
--
instance ContextClass LocalScope where
    lookup (GlobalScope bounds ctx) ident = case lookup_b bounds ident of
        Nothing -> lookup ctx ident
        just_type -> just_type
    lookup (InnerScope bounds ctx) ident = case lookup_b bounds ident of
        Nothing -> lookup ctx ident
        just_type -> just_type
    lookup (OuterScope bounds ctx) ident = case lookup_b bounds ident of
        Nothing -> lookup ctx ident
        just_type -> just_type


lookup_b :: [Bound] -> Identifier -> Maybe SoucType
lookup_b (b:bs) ident = case this_one b ident of
    Nothing -> lookup_b (bs) ident
    just_type -> just_type
lookup_b [] _ = Nothing

this_one :: Bound -> Identifier -> Maybe SoucType
this_one (Bound i t) ident = if i == ident then Just t else Nothing

add_bind :: LocalScope -> BindMayExist -> Bound -> Either TypeError LocalScope
add_bind ctx (BindMayExist modifiable) (Bound i t) = case lookup ctx i of
    Just existing_type -> if modifiable
        then if t == existing_type
            then Right ctx
            else Left (TypeMismatch existing_type t)
        else
            Left (MultipleDeclarations i)
    Nothing -> Right $ case ctx of
        GlobalScope binds rest -> GlobalScope (Bound i t : binds) rest
        OuterScope binds rest -> OuterScope (Bound i t : binds) rest
        InnerScope binds rest -> InnerScope (Bound i t : binds) rest

define_export :: LocalScope -> Bound -> Either TypeError LocalScope
define_export scope b = case scope of
--     InnerScope binds ctx -> define_export ctx b
--     OuterScope binds ctx -> define_export ctx b
    InnerScope _ _ -> error "should not define exports from inner scope"
    OuterScope _ _ -> error "should not define exports from within scope"
    GlobalScope binds ctx -> case b of
        Bound i _ -> case lookup_b binds i of
            Nothing -> Right (GlobalScope (b : binds) ctx)
            Just _ -> Left (MultipleDeclarations i)

remove_export_wrapper :: LocalScope -> Bound -> Either TypeError LocalScope
remove_export_wrapper scope  b = case scope of
    InnerScope _ _ -> error "should not remove exports from inner scope"
    OuterScope _ _ -> error "should not remove exports from within scope"
    GlobalScope bounds ctx -> case remove_export ctx b of
        Left err -> Left err
        Right ok_ctx -> Right (GlobalScope bounds ok_ctx)

remove_export :: ExportList -> Bound -> Either TypeError ExportList
-- remove_export (GlobalScope bounds ctx) b = case remove_export ctx b of
--     Left err -> Left err
--     Right ok_ctx -> Right (GlobalScope bounds ok_ctx)
remove_export (ExportList bounds ctx) (Bound i t) = case lookup_b bounds i of
    Just b_t | t == b_t -> Right (ExportList (filter (\(Bound b_i _) -> b_i /= i) bounds) ctx)
    Just b_t -> Left (TypeMismatch b_t t)
    Nothing -> Left (Undeclared i)

add_potential_export :: Bound -> Checker ()
-- add_potential_export :: ContextClass c => Bound -> State c (Either TypeError ())
add_potential_export bound = do
    ctx <- get
    case remove_export_wrapper ctx bound of
        Left (Undeclared _) -> insert (BindMayExist False) bound
        Left err -> pure (Left err)
        Right removed_ctx -> case define_export removed_ctx bound of
            Left err -> pure (Left err)
            Right new_ctx -> put new_ctx >> pure (Right ())


-- exports_remaining :: ContextClass c => c -> [Bound]
exports_remaining :: LocalScope -> [Bound]
-- exports_remaining (Scoped _ ctx) = exports_remaining ctx
exports_remaining (InnerScope _ ctx) = exports_remaining ctx
exports_remaining (OuterScope _ ctx) = exports_remaining ctx
exports_remaining (GlobalScope _ (ExportList [] _)) = []
exports_remaining (GlobalScope _ (ExportList bs _)) = bs
-- exports_remaining (Builtins _) = error "should be unreachable"

-- type Checker a = State Context (Either TypeError a)
-- type Checker a = ContextClass c => State c (Either TypeError a)

type Checker a = State LocalScope (Either TypeError a)

insert :: BindMayExist -> Bound -> Checker ()
insert = insert_local

insert_global :: Bound -> State LocalScope (Either TypeError ())
-- insert_global :: Bound -> State GlobalScope (Either TypeError ())
insert_global bound = do
    ctx <- get
    case (add_bind ctx (BindMayExist False) bound) of
        Left err -> pure (Left err)
        Right new_ctx -> put new_ctx >> pure (Right ())

-- insert_local :: BindMayExist -> Bound -> State LocalScope (Either TypeError ())
insert_local :: BindMayExist -> Bound -> Checker ()
insert_local modifiable bound = do
    ctx <- get
    case (add_bind ctx modifiable bound) of
        Left err -> pure (Left err)
        Right new_ctx -> put new_ctx >> pure (Right ())


builtins_ctx :: Builtins
builtins_ctx = Builtins [Bound "puts" (SoucRoutn (SoucType "String"))]
