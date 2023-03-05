{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- FIXME ideas
-- generalize `check` and `infer`, use typeclass or ADT

module SurCC.TypeChecker.Context (
    TopChecker,
    Checker,
    ExportList,
    ImportList,
    TypeConSet(..),
    GlobalScope(..),
    LocalScopes,
    ImmutMapping(..),
    run_top_checker,
    get_type,
    get_var,
    local_scope,
    local_scope_param,
    local_scope_main,
    new_scope,
    exit_scope,
    insert_global,
    insert_local,
    export_list,
    import_list,
    assert_type_exists,
    add_type_vars,
) where

import Control.Arrow ((|||), second)
import Control.Applicative ((<|>))
import Control.Monad (when, unless, foldM)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict (HashMap)
import Data.Set qualified as Set
import Data.Traversable (for)


import Prelude hiding (lookup)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader (Reader, ReaderT, runReaderT, runReader, ask)
import Control.Monad.State (evalStateT, runStateT, StateT, get, put)
import Control.Monad.Trans.Except (Except, runExcept, ExceptT, runExceptT)
import Data.Text (Text)

import SurCC.Common
import SurCC.Common.SoucTypes
import SurCC.Builtins (typeof_builtin)


type TopChecker a = StateT GlobalScope (
                        ReaderT (ImportList,ExportList) (
                            Except TypeError)) a

type Checker a = StateT LocalScopes (
                    ReaderT (ImportList,ExportList,GlobalScope) (
                        Except TypeError)) a


newtype ExportList = ExportList ImmutMapping
                deriving (Show,Semigroup,Monoid)

newtype ImportList = ImportList (Set.Set Identifier)
                deriving (Show,Semigroup,Monoid)


newtype ImmutMapping = ImmutMapping (HashMap Identifier SoucType)
                deriving (Show,Semigroup,Monoid)

type MutMapping = HashMap Identifier (SoucType, Mutability)


newtype TypeConSet = TypeConSet (HashMap TypeCon Refutable) deriving (Show,Semigroup,Monoid)

data GlobalScope = GlobalScope TypeConSet ImmutMapping
                deriving (Show)

data LocalScopes = LocalScopes TypeVarSet [MutMapping]
                deriving (Eq, Show)


newtype TypeVarSet = TypeVarSet (HashMap TypeVar Rigidity)
                deriving (Eq, Show, Semigroup, Monoid)

instance Semigroup GlobalScope where
    (GlobalScope types0 map0) <> (GlobalScope types1 map1) =
        GlobalScope (types0 <> types1) (map0 <> map1)

instance Monoid GlobalScope where
    mempty = GlobalScope mempty mempty

instance Semigroup LocalScopes where
    (LocalScopes types0 map0) <> (LocalScopes types1 map1) =
        LocalScopes (types0 <> types1) (map0 <> map1)

instance Monoid LocalScopes where
    mempty = LocalScopes mempty mempty

run_top_checker :: ImportList -> ExportList -> GlobalScope -> TopChecker a
                   -> Either TypeError (a,GlobalScope)
run_top_checker imports exports globals checker =
    run_mtl_stack globals (imports,exports) checker


run_mtl_stack :: s -> r -> StateT s (ReaderT r (Except TypeError)) a
                 -> Either TypeError (a,s)
run_mtl_stack s r func = runExcept (runReaderT (runStateT func s) r)


get_type :: Identifier -> Checker SoucType
get_type i = do
    (imports,_exports,globals) <- ask
    locals <- get
    case lookup i imports globals locals of
        Nothing -> throwError (Undeclared i)
        Just t -> pure t


get_var :: Identifier -> Checker SoucType
get_var i = do
    scopes <- get
    case lookup_locals_mutables i scopes of
        Just (t, Immut) -> throwError $ MutateImmutable i t
        Just (t, Mut) -> pure t
        Nothing -> throwError . MutateImmutable i =<< get_type i


lookup :: Identifier -> ImportList -> GlobalScope -> LocalScopes
          -> Maybe SoucType
lookup i imports globals locals =
    (lookup_imports i imports)
    <|> (lookup_globals i globals)
    <|> (lookup_locals_mutables i locals <&> fst)
    <|> (typeof_builtin i)



lookup_globals :: Identifier -> GlobalScope -> Maybe SoucType
lookup_globals i (GlobalScope _ (ImmutMapping bounds)) = Map.lookup i bounds


member_globals :: Identifier -> GlobalScope -> Bool
member_globals i globals = lookup_globals i globals & isJust


lookup_locals_mutables :: Identifier -> LocalScopes
                          -> Maybe (SoucType, Mutability)
lookup_locals_mutables i (LocalScopes _ scopes) = foldr f Nothing scopes
    where
        f scope next = Map.lookup i scope <|> next


lookup_exports :: Identifier -> ExportList -> Maybe SoucType
lookup_exports i (ExportList (ImmutMapping exports)) = Map.lookup i exports


member_exports :: Identifier -> ExportList -> Bool
member_exports i exports = lookup_exports i exports & isJust


lookup_imports :: Identifier -> ImportList -> Maybe SoucType
lookup_imports i imports = if member_imports i imports
    then Just SoucModuleType
    else Nothing


member_imports :: Identifier -> ImportList -> Bool
member_imports i (ImportList imports) = Set.member i imports


-- get_types :: TypeVar -> Checker (Maybe Refutable)
assert_type_exists :: SoucType -> Checker ()
assert_type_exists = \case
    SoucForAll v rig t -> insert_tvar v rig *> assert_type_exists t
    SoucTypeCon c args -> do
        assert_type_con_exists c
        for_ args assert_type_exists
    SoucTypeVar v args -> do
        assert_type_var_exists v
        for_ args assert_type_exists



-- like assert_type_exists, but converts its input
-- into a SoucType which does not contain any ForAlls
add_type_vars :: SoucType -> Checker SoucType
add_type_vars = \case
    SoucForAll v rig t -> do
        insert_tvar v rig
        add_type_vars t
    SoucTypeCon c args -> do
        assert_type_con_exists c
        new_args <- for args add_type_vars
        pure $ SoucTypeCon c new_args
    SoucTypeVar v args -> do
        assert_type_var_exists v
        new_args <- for args add_type_vars
        pure $ SoucTypeVar v new_args


insert_tvar_l :: (MonadError TypeError m)
                 => TypeVar -> Rigidity -> TypeVarSet -> m TypeVarSet
insert_tvar_l tv rig (TypeVarSet tvs) = if Map.member tv tvs
    then throwError $ MultipleTypeVarDecls tv
    else pure $ TypeVarSet (Map.insert tv rig tvs)


insert_tvar :: TypeVar -> Rigidity -> Checker ()
insert_tvar tv rig = get >>= \case
    LocalScopes (TypeVarSet tvs) vals -> if Map.member tv tvs
        then throwError $ MultipleTypeVarDecls tv
        else put $ LocalScopes (TypeVarSet (Map.insert tv rig tvs)) vals



assert_type_con_exists' :: (MonadError TypeError m)
                           => TypeCon -> TypeConSet -> m ()
assert_type_con_exists' con (TypeConSet type_set) = do
        unless (Map.member con type_set) $
            throwError (UnknownTypeCon con)


assert_type_con_exists :: TypeCon -> Checker ()
assert_type_con_exists con = do
        (_, _, GlobalScope type_set _) <- ask
        assert_type_con_exists' con type_set


assert_type_var_exists' :: (MonadError TypeError m)
                           => TypeVar -> TypeVarSet -> m ()
assert_type_var_exists' var (TypeVarSet type_var_set) = do
        unless (Map.member var type_var_set) $ throwError (UnknownTypeVar var)


assert_type_var_exists :: TypeVar -> Checker ()
assert_type_var_exists var = do
        LocalScopes type_var_set _ <- get
        assert_type_var_exists' var type_var_set


member_tvars :: TypeVar -> TypeVarSet -> Bool
member_tvars tv (TypeVarSet tvs) = Map.member tv tvs


export_list :: [ExportDecl] -> ExportList
export_list exports = ExportList $ ImmutMapping $
    Map.fromList (unwrap <$> exports)
    where
        unwrap (ExportDecl (Bound b t)) = (b,t)


import_list :: [ImportDecl] -> ImportList
import_list imports = ImportList $ Set.fromList (unwrap <$> imports)
    where
        unwrap = \case
            LibImport name -> Identifier name
            RelImport name -> Identifier name


local_scope_with :: [(Identifier,SoucType)] -> Checker a -> TopChecker a
local_scope_with list checker = do
    (imps,exps) <- ask
    globals <- get
    -- FIXME:
    -- this checks whether an identifier is already in use (globally)
    -- it does *not* check whether the same identifier appears twice in `list`
    for_ (list <&> fst) $ \i -> do
        when (isJust $ lookup i imps globals mempty) $
            throwError (MultipleDeclarations i)
        when (member_exports i exps) $
            throwError (ExportedLocal i)
    tv_set <- type_var_set (list <&> snd)
    run_mtl_stack (LocalScopes tv_set locals) (imps,exps,globals) checker
        & (throwError ||| confirm)
    where
        locals :: [MutMapping]
        locals = [list <&> second normalized <&> second (,Immut) & Map.fromList]
        confirm (result,LocalScopes _ end_state) = do
            when (end_state /= locals) $ -- redundant check for bug-prevention
                error "there is a bug in the compiler."
            pure result
        type_var_set :: [SoucType] -> TopChecker TypeVarSet
        type_var_set types = foldM go mempty types
            where
                go :: TypeVarSet -> SoucType -> TopChecker TypeVarSet
                go tvars = \case
                    SoucForAll v rig t -> do
                        tvs <- insert_tvar_l v rig tvars
                        go tvs t
                    SoucTypeCon con args -> do
                        GlobalScope type_cons _ <- get
                        assert_type_con_exists' con type_cons
                        foldM go tvars args
                    SoucTypeVar var args -> do
                        assert_type_var_exists' var tvars
                        foldM go tvars args


normalized :: SoucType -> SoucType
normalized = \case
    SoucForAll _v _rig t -> normalized t
    t -> t


local_scope :: Checker a -> TopChecker a
local_scope = local_scope_with []


local_scope_param :: Identifier -> SoucType -> Checker a -> TopChecker a
local_scope_param i t = local_scope_with [(i,t)]


local_scope_main :: MainParam -> Checker a -> TopChecker a
local_scope_main (MainParam
        (MainParamStdIn stdin)
        (MainParamStdOut stdout)
        (MainParamStdErr stderr)
        (MainParamProgName progname)
        (MainParamArgs args)
        (MainParamEnv env)
    ) = local_scope_with locals
    where
        locals :: [(Identifier,SoucType)]
        locals = include stdin "stdin" "InputStream"
                <> include stdout "stdout" "OutputStream"
                <> include stderr "stderr" "OutputStream"
                <> include progname "program_name" "String"
                <> include args "args" "FIXME"
                <> include env "env" "FIXME"

        include :: Bool -> Identifier -> Text
                     -> [(Identifier,SoucType)]
        include b name t = if b
            then [(name, (SoucType t))]
            else []


new_scope :: Checker ()
new_scope = get >>= \case
    LocalScopes tvs locals -> put (LocalScopes tvs (Map.empty : locals))


exit_scope :: Checker ()
exit_scope = get >>= \case
    (LocalScopes _ []) -> error "should be impossible"
    (LocalScopes tvs (_ : etc)) -> put (LocalScopes tvs etc)



insert_local :: Mutability -> Identifier -> SoucType -> Checker ()
insert_local modifiable i t = do
    (imports,exports,globals) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    when (member_exports i exports) $
        throwError (ExportedLocal i)
    when (member_globals i globals) $
        throwError (MultipleDeclarations i)
    -- FIXME this should also check the local scopes!
    (put =<<) $ get >>= \case
        LocalScopes _ [] -> error "should not be reachable"
        -- FIXME type var
        LocalScopes tvs (binds : etc) ->
            pure (LocalScopes tvs (Map.insert i (t, modifiable) binds : etc))


insert_global :: Bound -> TopChecker ()
insert_global (Bound i t) = do
    (imports,exports) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    for_ (lookup_exports i exports) $
        assert_equals t
    GlobalScope types (ImmutMapping globals) <- get
    if Map.member i globals
        then throwError (MultipleDeclarations i)
        else put (GlobalScope types (ImmutMapping (Map.insert i t globals)))


assert_equals :: (MonadError TypeError m) => SoucType -> SoucType -> m ()
assert_equals t0 t1 = unless (compat (t0,t1)) (throwError (TypeMismatch t0 t1))


compat :: (SoucType,SoucType) -> Bool
compat = \case
    (t0,t1) -> t0 == t1
