module SurCC.TypeChecker.Context (
    Checker,
    ExportList(..),
    LocalScope(..),
    lookup_identifier,
    lookup_constant,
    new_scope,
    new_param_scope,
    exit_scope,
    insert_immut,
    insert_mut,
    make_export_list,
    make_global_scope,
    add_potential_export,
    undefined_export,
) where

import Data.HashMap.Strict qualified as Map

import Prelude hiding (lookup)
import Control.Monad.State (State, get, put)
import Control.Monad.Trans.Except (ExceptT, throwE)

import SurCC.Common
import SurCC.Builtins (typeof_builtin)

type Checker a = ExceptT TypeError (State LocalScope) a

type Mapping = Map.HashMap (Either Identifier Constant) SoucType

newtype ExportList = ExportList Mapping

data LocalScope = GlobalScope Mapping ExportList
                | InnerScope
                    (Map.HashMap Identifier (SoucType, Mutability))
                    LocalScope

lookup_identifier :: Identifier -> LocalScope -> Maybe SoucType
lookup_identifier x ctx = lookup (Left x) ctx

lookup_constant :: Constant -> LocalScope -> Maybe SoucType
lookup_constant x ctx = lookup (Right x) ctx

lookup :: Either Identifier Constant -> LocalScope -> Maybe SoucType
lookup i = \case
    GlobalScope bounds ctx -> case Map.lookup i bounds of
        Nothing -> lookup_exports i ctx
        just_type -> just_type
    InnerScope bounds ctx -> case i of
        Left ident -> case Map.lookup ident bounds of
            Nothing -> lookup (Left ident) ctx
            just_type -> fst <$> just_type
        right -> lookup right ctx

lookup_mutable :: Identifier -> LocalScope -> Maybe (SoucType, Mutability)
lookup_mutable i = \case
    GlobalScope bounds ctx -> case Map.lookup (Left i) bounds of
        Nothing -> case lookup_exports (Left i) ctx of
            Nothing -> Nothing
            Just t -> Just (t, Immut)
        Just t -> Just (t, Immut)
    InnerScope bounds ctx -> case Map.lookup i bounds of
        Nothing -> lookup_mutable i ctx
        just_type -> just_type

lookup_exports :: Either Identifier Constant -> ExportList -> Maybe SoucType
lookup_exports i (ExportList bounds) = case Map.lookup i bounds of
    Nothing -> typeof_builtin i
    just_type -> just_type



make_export_list :: [ExportDecl] -> ExportList
make_export_list exports = ExportList $ Map.fromList (unwrap <$> exports)
    where
        unwrap (ExportDecl (Bound b t)) = (b,t)

make_global_scope :: Imports -> ExportList -> LocalScope
make_global_scope imps exps = GlobalScope import_map exps
    where
        import_map = Map.fromList (from_import <$> imps)
        from_import :: ImportDecl -> (Either Identifier Constant, SoucType)
        from_import = \case
            LibImport name -> wrap name
            RelImport name -> wrap name
        wrap txt = (Left (Identifier txt), SoucModuleType)

define_export :: LocalScope -> Bound -> Either TypeError LocalScope
define_export scope (Bound b t) = case scope of
    InnerScope _ _ -> error "should not define exports from inner scope"
    GlobalScope binds ctx -> case Map.lookup b binds of
            Nothing -> Right (GlobalScope (Map.insert b t binds) ctx)
            Just _ -> case b of
                Left i -> Left (MultipleDeclarations i)
                Right _ -> error "define_export a constant!?"

remove_export_wrapper :: LocalScope -> Either Identifier Constant -> SoucType
                        -> Either TypeError LocalScope
remove_export_wrapper scope b t = case scope of
    InnerScope _ _ -> error "should not remove exports from inner scope"
    GlobalScope bounds ctx -> case remove_export ctx b t of
        Left err -> Left err
        Right ok_ctx -> Right (GlobalScope bounds ok_ctx)

remove_export :: ExportList -> Either Identifier Constant -> SoucType
                -> Either TypeError ExportList
remove_export (ExportList bounds) b t = case Map.lookup b bounds of
    Just b_t | t == b_t -> Right $ ExportList $ Map.delete b bounds
    Just b_t -> Left (TypeMismatch b_t t)
    Nothing -> case b of
        Left i -> Left (Undeclared i)
        Right _ -> error "remove_export !?"


add_potential_export :: Bound -> Checker ()
add_potential_export = \case
    bound@(Bound (Left i) t) -> do
        ctx <- get
        case remove_export_wrapper ctx (Left i) t of
            Left (Undeclared _) -> insert_immut i t
            Left err -> throwE err
            Right removed_ctx -> case define_export removed_ctx bound of
                Left err -> throwE err
                Right new_ctx -> put new_ctx
    Bound (Right _) _ -> error "fixme haha"


undefined_export :: LocalScope -> Maybe Bound
undefined_export = \case
    InnerScope _ ctx -> undefined_export ctx
    GlobalScope _ (ExportList exports) -> case Map.toList exports of
        [] -> Nothing
        exps -> Just $ uncurry Bound $ head exps

new_scope :: Checker ()
new_scope = get >>= put . InnerScope Map.empty

new_param_scope :: Identifier -> SoucType -> Checker ()
new_param_scope i t = get >>= put . InnerScope (Map.singleton i (t, Immut))

exit_scope :: Checker ()
exit_scope = get >>= \case
    InnerScope _ inner -> put inner >> pure ()
    GlobalScope _ _ -> throwE (Undeclared "should be unreachable")


add_bind :: LocalScope -> Mutability -> Identifier -> SoucType
            -> Either TypeError LocalScope
add_bind ctx modifiable i t = case lookup_mutable i ctx of
    Just (existing_type, existing_mut) ->
        if (modifiable, existing_mut) == (Mut, Mut)
            then if t == existing_type
                then Right ctx
                else Left (TypeMismatch existing_type t)
        else
            Left (MultipleDeclarations i)
    Nothing -> Right $ case ctx of
        GlobalScope binds rest ->
            GlobalScope (Map.insert (Left i) t binds) rest
        InnerScope binds rest ->
            InnerScope ((Map.insert i (t, modifiable)) binds) rest

insert :: Mutability -> Identifier -> SoucType -> Checker ()
insert modifiable i t = do
    ctx <- get
    case add_bind ctx modifiable i t of
        Left err -> throwE err
        Right new_ctx -> put new_ctx

insert_mut :: Identifier -> SoucType -> Checker ()
insert_mut = insert Mut

insert_immut :: Identifier -> SoucType -> Checker ()
insert_immut = insert Immut
