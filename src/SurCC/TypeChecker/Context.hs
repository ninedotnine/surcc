{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}



-- FIXME ideas
-- generalize `check` and `infer`, use typeclass or ADT
-- make LocalScope and GlobalScope different types
--     change Checker state to use (GlobalScope,LocalScope)
-- delete many exports from this file
-- use MonadError


module SurCC.TypeChecker.Context (
    Checker,
    ExportList(..),
    ImportList,
    LocalScopes(..),
    run_checker,
    get_type,
--     lookup,
    new_scope,
    new_param_scope,
--     new_pattern_scope,
    new_main_scope,
    exit_scope,
    insert_global,
    insert_local,
    export_list,
    import_list,
    lookup_scopes_mutables,
) where

import Control.Arrow (second, (|||))
import Control.Applicative ((<|>))
import Control.Monad (when, unless)
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Maybe (isJust)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set


import Prelude hiding (lookup)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader (Reader, runReader, ask)
import Control.Monad.State (evalStateT, StateT, get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Text (Text)

import SurCC.Common
import SurCC.Builtins (typeof_builtin)

type Checker a = ExceptT TypeError (
                    StateT (GlobalScope,LocalScopes) (
                        Reader (ImportList,ExportList))) a


-- run_checker :: MonadError TypeError m
--             => ImportList -> ExportList -> Checker a -> m a
run_checker :: ImportList -> ExportList -> Checker a -> Either TypeError a
run_checker imports exports checker =
    runReader (evalStateT (runExceptT checker) empty_scope) (imports,exports)
    where
        empty_scope = (GlobalScope Map.empty, LocalScopes [])


newtype ExportList = ExportList ImmutMapping
                deriving (Show)

newtype ImportList = ImportList (Set.Set Identifier)
                deriving (Show)

type ImmutMapping = Map.Map Identifier SoucType

type MutMapping = Map.Map Identifier (SoucType, Mutability)

newtype GlobalScope = GlobalScope ImmutMapping
                deriving (Show)

-- data LocalScope = LocalScope MutMapping LocalScope
data LocalScopes = LocalScopes [MutMapping]
                deriving (Show)



get_type_mutables :: Identifier -> Checker (SoucType, Mutability)
get_type_mutables i = do
    (imports,_exports) <- ask
    (globals,locals) <- get
    case lookup_mutables i imports globals locals of
        Nothing -> throwError (Undeclared i)
        Just (t,m) -> pure (t,m)


get_type :: Identifier -> Checker SoucType
get_type i = get_type_mutables i <&> fst


lookup_mutables :: Identifier -> ImportList -> GlobalScope -> LocalScopes
                -> Maybe (SoucType, Mutability)
lookup_mutables i imports globals locals =
    (lookup_imports i imports <&> (,Immut))
    <|> (lookup_globals_mutables i globals)
    <|> (lookup_locals_mutables i locals)
    <|> (typeof_builtin i <&> (,Immut))


-- lookup :: Identifier -> ImportList -> LocalScopes -> Maybe SoucType
-- lookup i imports locals = lookup_mutables i imports locals <&> fst


lookup_globals_mutables :: Identifier -> GlobalScope
                        -> Maybe (SoucType, Mutability)
lookup_globals_mutables i (GlobalScope bounds) =
    Map.lookup i bounds <&> (,Immut)


lookup_locals_mutables :: Identifier -> LocalScopes
                       -> Maybe (SoucType, Mutability)
lookup_locals_mutables i  = \case
    LocalScopes [] -> Nothing
    LocalScopes (bounds : rest) ->
        Map.lookup i bounds <|> lookup_locals_mutables i (LocalScopes rest)


lookup_scopes_mutables :: Identifier -> GlobalScope -> LocalScopes
                       -> Maybe (SoucType, Mutability)
lookup_scopes_mutables i globals locals =
    lookup_locals_mutables i locals <|> lookup_globals_mutables i globals


lookup_scopes :: Identifier -> GlobalScope -> LocalScopes -> Maybe SoucType
-- lookup_scopes = lookup_scopes_mutables <&> fmap fst
lookup_scopes i globs locs = lookup_scopes_mutables i globs locs <&> fst
--     (Map.lookup i bounds <&> fst) <|> lookup_scopes i ctx
--     GlobalScope bounds -> Map.lookup i bounds


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



new_scope :: Checker ()
new_scope = get >>= (\(globs,LocalScopes locals) -> put (globs,LocalScopes (Map.empty : locals)))


new_param_scope :: Identifier -> SoucType -> Checker ()
-- new_param_scope i t = get >>= put . LocalScope (Map.singleton i (t, Immut))
new_param_scope i t = get >>= (\(globs,LocalScopes locals) -> put (globs,LocalScopes ((Map.singleton i (t, Immut)) : locals)))


new_pattern_scope :: [(Identifier,SoucType)] -> Checker ()
-- FIXME Map.fromList does not check for duplicate Identifiers
-- new_pattern_scope binds = get >>= put . LocalScope new_map
--     where
--         new_map = Map.fromList (binds <&> second (,Immut))
new_pattern_scope binds = get >>= (\(globs,LocalScopes locals) -> put $ (globs, LocalScopes (new_map : locals)))
    where
        new_map = Map.fromList (binds <&> second (,Immut))


new_main_scope :: MainParam -> Checker ()
new_main_scope (MainParam
        (MainParamStdIn stdin)
        (MainParamStdOut stdout)
        (MainParamStdErr stderr)
        (MainParamProgName progname)
        (MainParamArgs args)
        (MainParamEnv env)
    ) = do
    let list = include stdin "stdin" "InputStream"
            <> include stdout "stdout" "OutputStream"
            <> include stderr "stderr" "OutputStream"
            <> include progname "program_name" "String"
            <> include args "args" "FIXME"
            <> include env "env" "FIXME"
--     get >>= put . LocalScope (Map.fromList list)
    get >>= (\(globs,LocalScopes locals) -> put $ (globs, LocalScopes (Map.fromList list : locals)))

    where include :: Bool -> Identifier -> Text
                     -> [(Identifier,(SoucType,Mutability))]
          include b name t = if b
            then [(name, (SoucType t (SoucKind 0), Immut))]
            else []


-- FIXME use second from Arrow for these
exit_scope :: Checker ()
exit_scope = get >>= \case
    (_,LocalScopes []) -> error "should be impossible"
    (globs,LocalScopes (_ : etc)) -> put (globs,LocalScopes etc)
--     GlobalScope _ -> throwError (Undeclared "should be unreachable")


insert_local :: Mutability -> Identifier -> SoucType -> Checker ()
insert_local modifiable i t = do
    (imports,exports) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    when (member_exports i exports) $
        throwError (ExportedLocal i)
    (put =<<) $ get >>= \case
--     (put =<<) $ (get <&> snd) >>= \case
--         GlobalScope binds -> do
--             when (modifiable == Mut) $
--                 error "should not be reachable"
--             pure $ GlobalScope (Map.insert i t binds)
        (_, LocalScopes []) -> error "should not be reachable"
        (globs, LocalScopes (binds : etc)) ->
            pure (globs,
                  LocalScopes (Map.insert i (t, modifiable) binds : etc))


insert_global :: Bound -> Checker ()
insert_global (Bound i t) = do
    (imports,exports) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    case lookup_exports i exports of
        Just exp_t -> assert_equals t exp_t
        Nothing -> pure ()
    get >>= \case
--     get <&> fst >>= \case
--        GlobalScope globals -> define_export globals i t
--                              & (throwError ||| put)
--         LocalScope _ _ -> error "should not be adding exports from here."
        (GlobalScope globals, locals) -> do
        globs <- define_export globals i t
        put (globs,locals)



define_export :: MonadError TypeError m
              => ImmutMapping -> Identifier -> SoucType -> m GlobalScope
define_export globals b t = if Map.member b globals
    then throwError (MultipleDeclarations b)
    else pure (GlobalScope (Map.insert b t globals))


assert_equals :: (MonadError TypeError m) => SoucType -> SoucType -> m ()
assert_equals t0 t1 = unless (t0 == t1) (throwError (TypeMismatch t0 t1))
