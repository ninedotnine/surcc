module TypeChecker.Context (
    Context(..),
    Bound(..),
    lookup,
    add_bind,
    empty_context,
) where

import Prelude hiding (lookup)

import Common
import Parser.Expr.ExprTypes

data Bound = Bound Identifier TypeName

instance Show Bound where
    show (Bound (Identifier i) (TypeName t)) = "Bound " ++ i ++ ": " ++ t

data Context = Global [Bound]
             | Scoped [Bound] Context
    deriving Show

lookup :: Context -> Identifier -> Maybe TypeName
lookup (Scoped bounds ctx) ident = case lookup_b bounds ident of
    Nothing -> lookup ctx ident
    just_type -> just_type
lookup (Global bounds) ident = lookup_b bounds ident


lookup_b :: [Bound] -> Identifier -> Maybe TypeName
lookup_b (b:bs) ident = case this_one b ident of
    Nothing -> lookup_b (bs) ident
    just_type -> just_type
lookup_b [] _ = Nothing

this_one :: Bound -> Identifier -> Maybe TypeName
this_one (Bound i t) ident = if i == ident then Just t else Nothing

add_bind :: Context -> Bound -> Either TypeError Context
add_bind ctx (Bound i t) = case lookup ctx i of
    Just _ -> Left (MultipleDeclarations i)
    Nothing -> Right $ case ctx of
        Global binds -> Global (Bound i t : binds)
        Scoped binds rest -> Scoped (Bound i t : binds) rest

empty_context :: Context
empty_context = Global []
