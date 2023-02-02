{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SurCC.CodeGen.ExprGen (
    gen_expr,
    gen_identifier,
) where

import SurCC.Parser.ExprParser (
    ExprTree(..),
    Term(..),
    Operator(..),
    PrefixOperator(..)
    )

import SurCC.Builtins (gen_builtin_data, gen_builtin_identifier)
import SurCC.Common (Identifier(..), Pattern(..), Literal(..), Guard(..))
import SurCC.CodeGen.Common

import Control.Monad.State (get, put)
import Control.Monad.Writer (tell)
import Data.Maybe (fromMaybe)
import Data.Foldable (fold)
import Data.Functor
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

gen_expr :: ExprTree -> Generator Text
gen_expr expr = do
    (decls, text) <- generate_expr expr
    tell $ gen_decls decls
    pure text


newtype Decls = Decls (Set Identifier)
    deriving (Eq, Ord, Show, Semigroup, Monoid)


-- first is declarations, second is the expression itself
generate_expr :: ExprTree -> Generator (Decls,Text)
generate_expr = \case
    Leaf e -> (mempty,) <$> generate_term e
    Signed e _ -> generate_expr e
    Twig op e -> do
        pref <- generate_prefix_expr op
        (decls, expr) <- generate_expr e
        pure $ (decls , pref <> expr <> "// fixme\n")
    Match scrutinee branches -> do
        (decls, expr) <- generate_match_expr scrutinee branches
        tell $ gen_decls decls
        let failure = generate_literal (LitInt 0) -- FIXME default to 0
        pure $ (mempty, expr <> " /* fail clause */ " <> failure <> "\n)")
    Branch op x y -> case op of
        Plus              ->  (,) mempty <$> gen_call "_souc_sum(" x y
        Minus             ->  (,) mempty <$> gen_call "_souc_difference(" x y
        Splat             ->  (,) mempty <$> gen_call "_souc_product(" x y
        FieldDiv          ->  (,) mempty <$> gen_call "_souc_quotient(" x y
        FloorDiv          ->  undefined
        Modulo            ->  (,) mempty <$> gen_call "_souc_remainder(" x y
        Hihat             ->  undefined -- FIXME C doesn't have ^
        Equals            ->  (,) mempty <$>
                                gen_call "_souc_is_equal_integer(" x y
        NotEquals         ->  (,) mempty <$>
                                gen_call "_souc_is_unequal_integer(" x y
        RegexMatch        ->  undefined
        GreaterThan       ->  (,) mempty <$> gen_call "_souc_is_greater(" x y
        LesserThan        ->  (,) mempty <$> gen_call "_souc_is_lesser(" x y
        And               ->  (,) mempty <$> gen_call "_souc_conjunction(" x y
        Or                ->  (,) mempty <$> gen_call "_souc_disjunction(" x y
        Xor               ->  undefined
        In                ->  undefined
        Comma             ->  gen_tuple x y
        Iff               ->  undefined
        FromMaybe         ->  undefined
        Prepend           ->  undefined
        Append            ->  undefined
        Combine           ->  undefined
        Index             ->  undefined
        Lookup            ->  undefined
        Apply             ->  (,) mempty <$> gen_apply x y
        FlipApply         ->  (,) mempty <$> gen_apply y x
        Map               ->  undefined
        FlipMap           ->  undefined
        Applicative       ->  undefined
        FlipApplicative   ->  undefined
        SequenceRight     ->  undefined
        SequenceLeft      ->  undefined
        BindRight         ->  undefined
        BindLeft          ->  undefined


gen_apply :: ExprTree -> ExprTree -> Generator Text
gen_apply x y = do
    (x_decls, gx) <- generate_expr x
    (y_decls, gy) <- generate_expr y
    pure $ gen_decls x_decls <> gen_decls y_decls <> gx <> "(" <> gy <> ")"

gen_call :: Text -> ExprTree -> ExprTree -> Generator Text
gen_call s x y = do
    (x_decls, gx) <- generate_expr x
    (y_decls, gy) <- generate_expr y
    pure $ gen_decls x_decls <> gen_decls y_decls <> s <> gx <> "," <> gy <> ")"

gen_tuple :: ExprTree -> ExprTree -> Generator (Decls,Text)
gen_tuple x y = do
    (x_decls, gx) <- generate_expr x
    (y_decls, gy) <- generate_expr y
    (CIdentifier pair) <- get_next_id
    -- fixme: this works for static storage, but not automatic storage
    -- space for the return value should be allocated outside of the function
    tell $ "struct _souc_pair " <> pair <> ";\n"

    pure $ (x_decls <> y_decls,
            "(" <> pair <> " = (struct _souc_pair) {.first = "
             <> gx <> ", .second = " <> gy <> "},\n"
             <> "(union _souc_obj) {._souc_pair = &" <> pair <> "})")

generate_term :: Term -> Generator Text
generate_term = \case
    Lit l -> pure $ generate_literal l
    Var v -> pure $ gen_identifier v
    Constructor s -> case gen_builtin_data s of
        Just output -> pure output
        Nothing -> pure "47" -- fixme hehe


generate_literal :: Literal -> Text
generate_literal = \case
    LitInt i -> "(union _souc_obj) {._souc_int = "
                <> Text.pack (show i) <> "}"
    LitChar c -> "(union _souc_obj) {._souc_char =  \'"
                 <> Text.singleton c <> "\'}"
    LitString s -> "(union _souc_obj) {._souc_str = \""
                   <> s <> "\"}"


generate_prefix_expr :: PrefixOperator -> Generator Text
generate_prefix_expr = \case
    GetAddr    -> pure "&"
    Deref      -> pure "*"
    Negate     -> pure "-"
    ToString   -> undefined
    Pure       -> undefined
    Join       -> undefined


generate_match_expr :: ExprTree -> [(Pattern,Maybe Guard,ExprTree)]
                       -> Generator (Decls,Text)
generate_match_expr scrutinee branches = do
    (decls, expr) <- generate_expr scrutinee
    alloc <- get_next_id
    tell $ "union _souc_obj " <> gen_c_identifier alloc <> ";\n"
    arms <- mconcat <$> traverse (generate_case alloc) branches
    pure $ (decls, "(\n" <> gen_c_identifier alloc <> " = " <> expr <> ",\n") <> arms


generate_case :: CIdentifier -> (Pattern,Maybe Guard,ExprTree)
                 -> Generator (Decls,Text)
generate_case alloc (pat,m_guard,expr) = do
    (decls, e) <- generate_expr expr

    p <- case pat of
        PatLit l -> pure $ (mempty, "(_souc_is_equal_integer(("
                                    <> gen_c_identifier alloc
                                    <> "),(" <> generate_literal l
                                    <> "))._souc_bool")
        PatBinding i -> do
            -- finish with `, true` in case there is no guard clause
            pure $ (new_decl i, "(" <> gen_identifier i <> " = "
                                <> gen_c_identifier alloc <> ", true")

    g <- case m_guard of
        Nothing -> pure mempty
        Just (Guard guar) -> do
            (guard_decls, guard_clause) <- generate_expr guar
            pure $ (guard_decls, ", (" <> guard_clause <> ")._souc_bool")

    let closer = ") ?\n(" <> e <> ") :\n"
    pure $ p <> g <> (decls,closer)


gen_c_identifier :: CIdentifier -> Text
gen_c_identifier (CIdentifier i) = i


gen_identifier :: Identifier -> Text
gen_identifier i = fromMaybe (prepend i) (gen_builtin_identifier i)
    where
        prepend (Identifier name) = "_souc_user_" <> name


get_next_id :: Generator CIdentifier
get_next_id = do
    n <- get
    put (n+1)
    pure $ CIdentifier $ Text.pack $ 'v' : show n


gen_decls :: Decls -> Text
gen_decls (Decls ids) = fold (Set.map declaration ids)
    where
        declaration i = "union _souc_obj " <> gen_identifier i <> ";\n"


new_decl :: Identifier -> Decls
new_decl i = Decls (Set.singleton i)
