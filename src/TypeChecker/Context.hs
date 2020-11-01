module TypeChecker.Context (
    Context(..),
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

-- FIXME split this into different data types
data Context = Builtins [Bound]
             | Exported [Bound] Context
             | Global [Bound] Context
             | Scoped [Bound] Context
    deriving Show

lookup :: Context -> Identifier -> Maybe SoucType
lookup (Scoped bounds ctx) ident = case lookup_b bounds ident of
    Nothing -> lookup ctx ident
    just_type -> just_type
lookup (Global bounds ctx) ident = case lookup_b bounds ident of
    Nothing -> lookup ctx ident
    just_type -> just_type
lookup (Exported bounds ctx) ident = case lookup_b bounds ident of
    Nothing -> lookup ctx ident
    just_type -> just_type
lookup (Builtins bounds) ident = lookup_b bounds ident


lookup_b :: [Bound] -> Identifier -> Maybe SoucType
lookup_b (b:bs) ident = case this_one b ident of
    Nothing -> lookup_b (bs) ident
    just_type -> just_type
lookup_b [] _ = Nothing

this_one :: Bound -> Identifier -> Maybe SoucType
this_one (Bound i t) ident = if i == ident then Just t else Nothing

add_bind :: Context -> BindMayExist -> Bound -> Either TypeError Context
add_bind ctx (BindMayExist modifiable) (Bound i t) = case lookup ctx i of
    Just existing_type -> if modifiable
        then if t == existing_type
            then Right ctx
            else Left (TypeMismatch existing_type t)
        else
            Left (MultipleDeclarations i)
    Nothing -> Right $ case ctx of
        Builtins _ -> error "don't add to built-ins!"
        Exported _ _ -> error "don't add to exported!"
        Global binds rest -> Global (Bound i t : binds) rest
        Scoped binds rest -> Scoped (Bound i t : binds) rest

define_export :: Context -> Bound -> Either TypeError Context
define_export (Scoped _ _) _ = error "don't call this from non-global scope"
define_export (Global bounds ctx) (Bound i t) = case lookup_b bounds i of
    Nothing -> Right (Global (Bound i t : bounds) ctx)
    Just _ -> Left (MultipleDeclarations i)
define_export (Exported _ _) _ = error "do i need this?"
-- define_export (Exported bounds ctx) (Bound i t) = case bounds of
--     (Bound b_i b_t : _) | b_i == i -> undefined
--     (b : binds) -> define_export binds (Bound i t)
define_export (Builtins _) _ = error "don't call this without globals"


remove_export :: Context -> Bound -> Either TypeError Context
remove_export (Scoped _ _) _ = error "don't call this from non-global scope"
remove_export (Global bounds ctx) b = case remove_export ctx b of
    Left err -> Left err
    Right ok_ctx -> Right (Global bounds ok_ctx)
remove_export (Exported bounds ctx) (Bound i t) = case lookup_b bounds i of
    Just b_t | t == b_t -> Right (Exported (filter (\(Bound b_i _) -> b_i /= i) bounds) ctx)
    Just b_t -> Left (TypeMismatch b_t t)
    Nothing -> Left (Undeclared i)
remove_export (Builtins _) _ = error "don't call this without globals cuck"

add_potential_export :: Bound -> Checker ()
add_potential_export bound = do
    ctx <- get
    case remove_export ctx bound of
        Left (Undeclared _) -> insert (BindMayExist False) bound
        Left err -> pure (Left err)
        Right removed_ctx -> case define_export removed_ctx bound of
            Left err -> pure (Left err)
            Right new_ctx -> put new_ctx >> pure (Right ())


exports_remaining :: Context -> [Bound]
exports_remaining (Scoped _ ctx) = exports_remaining ctx
exports_remaining (Global _ ctx) = exports_remaining ctx
exports_remaining (Exported [] _) = []
exports_remaining (Exported bs _) = bs
exports_remaining (Builtins _) = error "should be unreachable"

type Checker a = State Context (Either TypeError a)

insert :: BindMayExist -> Bound -> Checker ()
insert modifiable bound = do
    ctx <- get
    case (add_bind ctx modifiable bound) of
        Left err -> pure (Left err)
        Right new_ctx -> put new_ctx >> pure (Right ())

builtins_ctx :: Context
builtins_ctx = Builtins [Bound "puts" (SoucRoutn (SoucType "String"))]
