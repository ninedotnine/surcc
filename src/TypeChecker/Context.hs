module TypeChecker.Context (
    Context(..),
    Bound(..),
    lookup,
    add_bind,
    BindMayExist(..),
    empty_context,
    Checker,
    insert,
) where

import Prelude hiding (lookup)
import Control.Monad.State

import Common
import Parser.Expr.ExprTypes

data Bound = Bound Identifier SoucType

newtype BindMayExist = BindMayExist Bool

instance Show Bound where
    show (Bound (Identifier i) t) = "Bound " ++ i ++ ": " ++ show t

data Context = Global [Bound]
             | Scoped [Bound] Context
    deriving Show

lookup :: Context -> Identifier -> Maybe SoucType
lookup (Scoped bounds ctx) ident = case lookup_b bounds ident of
    Nothing -> lookup ctx ident
    just_type -> just_type
lookup (Global bounds) ident = lookup_b bounds ident


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
        Global binds -> Global (Bound i t : binds)
        Scoped binds rest -> Scoped (Bound i t : binds) rest

type Checker a = State Context (Either TypeError a)

insert :: BindMayExist -> Bound -> Checker ()
insert modifiable bound = do
    ctx <- get
    case (add_bind ctx modifiable bound) of
        Left err -> pure (Left err)
        Right new_ctx -> put new_ctx >> pure (Right ())

empty_context :: Context
empty_context = Global []
