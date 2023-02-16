{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

-- FIXME ideas
-- generalize `check` and `infer`, use typeclass or ADT

module SurCC.TypeChecker.Context (
    TopChecker,
    Checker,
    ExportList(..),
    ImportList,
    GlobalScope(..),
    LocalScopes(..),
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
) where

import Control.Arrow ((|||))
import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Data.Foldable (for_)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict (HashMap)
import Data.Set qualified as Set


import Prelude hiding (lookup)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader (Reader, runReader, ask)
import Control.Monad.State (evalStateT, StateT, get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text (Text)

import SurCC.Common
import SurCC.Builtins (typeof_builtin)


type TopChecker a = ExceptT TypeError (
                        StateT (GlobalScope) (
                            Reader (ImportList,ExportList))) a

type Checker a = ExceptT TypeError (
                    StateT (LocalScopes) (
                        Reader (ImportList,ExportList,GlobalScope))) a


newtype ExportList = ExportList ImmutMapping
                deriving (Show)

newtype ImportList = ImportList (Set.Set Identifier)
                deriving (Show)


type ImmutMapping = HashMap Identifier SoucType

type MutMapping = HashMap Identifier (SoucType, Mutability)


newtype GlobalScope = GlobalScope ImmutMapping
                deriving (Show)

newtype LocalScopes = LocalScopes [MutMapping]
                deriving (Show)


run_top_checker :: ImportList -> ExportList -> GlobalScope -> TopChecker a
                   -> Either TypeError a
run_top_checker imports exports scope checker =
    runReader (evalStateT (runExceptT checker) scope) (imports,exports)


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
lookup_globals i (GlobalScope bounds) = Map.lookup i bounds


member_globals :: Identifier -> GlobalScope -> Bool
member_globals i globals = lookup_globals i globals & isJust


lookup_locals_mutables :: Identifier -> LocalScopes
                          -> Maybe (SoucType, Mutability)
lookup_locals_mutables i (LocalScopes scopes) = foldr f Nothing scopes
    where
        f scope next = Map.lookup i scope <|> next


lookup_exports :: Identifier -> ExportList -> Maybe SoucType
lookup_exports i (ExportList exports) = Map.lookup i exports


member_exports :: Identifier -> ExportList -> Bool
member_exports i exports = lookup_exports i exports & isJust


lookup_imports :: Identifier -> ImportList -> Maybe SoucType
lookup_imports i imports = if member_imports i imports
    then Just SoucModuleType
    else Nothing


member_imports :: Identifier -> ImportList -> Bool
member_imports i (ImportList imports) = Set.member i imports


export_list :: [ExportDecl] -> ExportList
export_list exports = ExportList $ Map.fromList (unwrap <$> exports)
    where
        unwrap (ExportDecl (Bound b t)) = (b,t)


import_list :: [ImportDecl] -> ImportList
import_list imports = ImportList $ Set.fromList (unwrap <$> imports)
    where
        unwrap = \case
            LibImport name -> Identifier name
            RelImport name -> Identifier name


local_scope_with :: LocalScopes -> Checker a -> TopChecker a
local_scope_with locals checker = do
    (imps,exps) <- ask
    globals <- get
    runReader (evalStateT (runExceptT checker) locals) (imps,exps,globals)
        & (throwError ||| pure)


local_scope :: Checker a -> TopChecker a
local_scope checker = local_scope_with locals checker
    where locals = LocalScopes [Map.empty]


local_scope_param :: Identifier -> SoucType -> Checker a -> TopChecker a
local_scope_param i t checker = local_scope_with locals checker
    where locals = LocalScopes [Map.singleton i (t, Immut)]


local_scope_main :: MainParam -> Checker a -> TopChecker a
local_scope_main (MainParam
        (MainParamStdIn stdin)
        (MainParamStdOut stdout)
        (MainParamStdErr stderr)
        (MainParamProgName progname)
        (MainParamArgs args)
        (MainParamEnv env)
    ) checker = local_scope_with locals checker
    where
        locals = LocalScopes [Map.fromList list]
        list = include stdin "stdin" "InputStream"
                <> include stdout "stdout" "OutputStream"
                <> include stderr "stderr" "OutputStream"
                <> include progname "program_name" "String"
                <> include args "args" "FIXME"
                <> include env "env" "FIXME"

        include :: Bool -> Identifier -> Text
                     -> [(Identifier,(SoucType,Mutability))]
        include b name t = if b
            then [(name, (SoucType t (SoucKind 0), Immut))]
            else []


new_scope :: Checker ()
new_scope = get >>= \case
    LocalScopes locals -> put (LocalScopes (Map.empty : locals))


exit_scope :: Checker ()
exit_scope = get >>= \case
    (LocalScopes []) -> error "should be impossible"
    (LocalScopes (_ : etc)) -> put (LocalScopes etc)


insert_local :: Mutability -> Identifier -> SoucType -> Checker ()
insert_local modifiable i t = do
    (imports,exports,globals) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    when (member_exports i exports) $
        throwError (ExportedLocal i)
    when (member_globals i globals) $
        throwError (MultipleDeclarations i)
    (put =<<) $ get >>= \case
        LocalScopes [] -> error "should not be reachable"
        LocalScopes (binds : etc) ->
            pure (LocalScopes (Map.insert i (t, modifiable) binds : etc))


insert_global :: Bound -> TopChecker ()
insert_global (Bound i t) = do
    (imports,exports) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    for_ (lookup_exports i exports) $
        assert_equals t
    GlobalScope globals <- get
    if Map.member i globals
        then throwError (MultipleDeclarations i)
        else put (GlobalScope (Map.insert i t globals))


assert_equals :: (MonadError TypeError m) => SoucType -> SoucType -> m ()
assert_equals t0 t1 = unless (t0 == t1) (throwError (TypeMismatch t0 t1))
