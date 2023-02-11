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
    LocalScope(..),
    run_checker,
    get_type,
    lookup,
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
                    StateT LocalScope (
                        Reader (ImportList,ExportList))) a


-- run_checker :: MonadError TypeError m
--             => ImportList -> ExportList -> Checker a -> m a
run_checker :: ImportList -> ExportList -> Checker a -> Either TypeError a
run_checker imports exports checker =
    runReader (evalStateT (runExceptT checker) empty_scope) (imports,exports)
    where
        empty_scope = GlobalScope (Map.empty)


newtype ExportList = ExportList ImmutMapping
                deriving (Show)

newtype ImportList = ImportList (Set.Set Identifier)
                deriving (Show)

type ImmutMapping = Map.Map Identifier SoucType

type MutMapping = Map.Map Identifier (SoucType, Mutability)

data LocalScope = GlobalScope ImmutMapping
                | InnerScope MutMapping LocalScope
                deriving (Show)



get_type_mutables :: Identifier -> Checker (SoucType, Mutability)
get_type_mutables i = do
    (imports,_exports) <- ask
    ctx <- get
    case lookup_mutables i imports ctx of
        Nothing -> throwError (Undeclared i)
        Just (t,m) -> pure (t,m)


get_type :: Identifier -> Checker SoucType
get_type i = get_type_mutables i <&> fst


lookup_mutables :: Identifier -> ImportList -> LocalScope
                -> Maybe (SoucType, Mutability)
lookup_mutables i imports ctx = (lookup_imports i imports <&> (,Immut))
                            <|> (lookup_scopes_mutables i ctx)
                            <|> (typeof_builtin i <&> (,Immut))


lookup :: Identifier -> ImportList -> LocalScope -> Maybe SoucType
lookup i imports ctx = lookup_mutables i imports ctx <&> fst


lookup_scopes_mutables :: Identifier -> LocalScope
                       -> Maybe (SoucType, Mutability)
lookup_scopes_mutables i = \case
    GlobalScope bounds -> Map.lookup i bounds <&> (,Immut)
    InnerScope bounds ctx -> Map.lookup i bounds
                         <|> lookup_scopes_mutables i ctx


lookup_scopes :: Identifier -> LocalScope -> Maybe SoucType
lookup_scopes i = \case
    GlobalScope bounds -> Map.lookup i bounds
    InnerScope bounds ctx -> (Map.lookup i bounds <&> fst)
                         <|> lookup_scopes i ctx


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
new_scope = get >>= put . InnerScope Map.empty


new_param_scope :: Identifier -> SoucType -> Checker ()
new_param_scope i t = get >>= put . InnerScope (Map.singleton i (t, Immut))


new_pattern_scope :: [(Identifier,SoucType)] -> Checker ()
-- FIXME Map.fromList does not check for duplicate Identifiers
new_pattern_scope binds = get >>= put . InnerScope new_map
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
    get >>= put . InnerScope (Map.fromList list)

    where include :: Bool -> Identifier -> Text
                     -> [(Identifier,(SoucType,Mutability))]
          include b name t = if b
            then [(name, (SoucType t (SoucKind 0), Immut))]
            else []


exit_scope :: Checker ()
exit_scope = get >>= \case
    InnerScope _ inner -> put inner
    GlobalScope _ -> throwError (Undeclared "should be unreachable")


insert_local :: Mutability -> Identifier -> SoucType -> Checker ()
insert_local modifiable i t = do
    (imports,exports) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    when (member_exports i exports) $
        throwError (ExportedLocal i)
    (put =<<) $ get >>= \case
        GlobalScope binds -> do
            when (modifiable == Mut) $
                error "should not be reachable"
            pure $ GlobalScope (Map.insert i t binds)
        InnerScope binds rest ->
            pure $ InnerScope ((Map.insert i (t, modifiable)) binds) rest


insert_global :: Bound -> Checker ()
insert_global (Bound i t) = do
    (imports,exports) <- ask
    when (member_imports i imports) $
        throwError (MultipleDeclarations i)
    case lookup_exports i exports of
        Just exp_t -> assert_equals t exp_t
        Nothing -> pure ()
    get >>= \case
        GlobalScope globals -> define_export globals i t
                             & (throwError ||| put)
        InnerScope _ _ -> error "should not be adding exports from here."


define_export :: MonadError TypeError m
              => ImmutMapping -> Identifier -> SoucType -> m LocalScope
define_export globals b t = if Map.member b globals
    then throwError (MultipleDeclarations b)
    else pure (GlobalScope (Map.insert b t globals))


assert_equals :: (MonadError TypeError m) => SoucType -> SoucType -> m ()
assert_equals t0 t1 = unless (t0 == t1) (throwError (TypeMismatch t0 t1))
