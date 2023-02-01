module SurCC.CodeGen.ExprGen (
    generate_expr,
    generate_identifier,
) where

import SurCC.Parser.ExprParser (
    ExprTree(..),
    Term(..),
    Operator(..),
    PrefixOperator(..)
    )

import SurCC.Builtins (gen_builtin_data, gen_builtin_identifier)
import SurCC.Common (Identifier(..), Pattern(..), Literal(..))
import SurCC.CodeGen.Common

import Control.Monad.State (get, put)
import Control.Monad.Writer (tell)
import Data.Functor
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text


-- first is declarations, second is the expression itself
generate_expr :: ExprTree -> Generator (Text,Text)
generate_expr = \case
    Leaf e -> (,) "" <$> generate_term e
    Signed e _ -> generate_expr e
    Twig op e -> do
        pref <- generate_prefix_expr op
        (decls, expr) <- generate_expr e
        pure $ (decls , pref <> expr <> "// fixme\n")
    Match scrutinee branches -> do
        (decls, e) <- generate_expr scrutinee
        (case_decls, cases) <- generate_cases e branches
        let failure = generate_literal (LitInt 0) -- FIXME default to 0
        pure $ (decls <> case_decls,
                cases <> " " <> failure)

    Branch op x y -> case op of
        Plus              ->  (,) "" <$> gen_call "_souc_sum(" x y
        Minus             ->  (,) "" <$> gen_call "_souc_difference(" x y
        Splat             ->  (,) "" <$> gen_call "_souc_product(" x y
        FieldDiv          ->  (,) "" <$> gen_call "_souc_quotient(" x y
        FloorDiv          ->  undefined
        Modulo            ->  (,) "" <$> gen_call "_souc_remainder(" x y
        Hihat             ->  undefined -- FIXME C doesn't have ^
        Equals            ->  (,) "" <$>
                                gen_call "_souc_is_equal_integer(" x y
        NotEquals         ->  (,) "" <$>
                                gen_call "_souc_is_unequal_integer(" x y
        RegexMatch        ->  undefined
        GreaterThan       ->  (,) "" <$> gen_call "_souc_is_greater(" x y
        LesserThan        ->  (,) "" <$> gen_call "_souc_is_lesser(" x y
        And               ->  (,) "" <$> gen_call "_souc_conjunction(" x y
        Or                ->  (,) "" <$> gen_call "_souc_disjunction(" x y
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
        Apply             ->  (,) "" <$> gen_apply x y
        FlipApply         ->  (,) "" <$> gen_apply y x
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
    pure $ x_decls <> y_decls <> gx <> "(" <> gy <> ")"

gen_call :: Text -> ExprTree -> ExprTree -> Generator Text
gen_call s x y = do
    (x_decls, gx) <- generate_expr x
    (y_decls, gy) <- generate_expr y
    pure $ x_decls <> y_decls <> s <> gx <> "," <> gy <> ")"

gen_tuple :: ExprTree -> ExprTree -> Generator (Text,Text)
gen_tuple x y = do
    (x_decls, gx) <- generate_expr x
    (y_decls, gy) <- generate_expr y
    (CIdentifier pair) <- get_next_id
    -- fixme: this works for static storage, but not automatic storage
    -- space for the return value should be allocated outside of the function
    tell $ "struct _souc_pair " <> pair <> ";\n"
    pure $ (x_decls <> y_decls <> pair <> " = (struct _souc_pair) {.first = "
           <> gx <> ", .second = " <> gy <> "};\n" ,
           "(union _souc_obj) {._souc_pair = &" <> pair <> "}")


generate_term :: Term -> Generator Text
generate_term = \case
    Lit l -> pure $ generate_literal l
    Var v -> pure $ generate_identifier_text v
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


generate_identifier :: Identifier -> CIdentifier
generate_identifier = CIdentifier . generate_identifier_text

generate_identifier_text :: Identifier -> Text
generate_identifier_text i = fromMaybe (prepend i) (gen_builtin_identifier i)
    where
        prepend (Identifier name) = "_souc_user_" <> name

generate_prefix_expr :: PrefixOperator -> Generator Text
generate_prefix_expr = \case
    GetAddr    -> pure "&"
    Deref      -> pure "*"
    Negate     -> pure "-"
    ToString   -> undefined
    Pure       -> undefined
    Join       -> undefined


generate_cases :: Text -> [(Pattern,ExprTree)] -> Generator (Text,Text)
generate_cases expr branches =
    mconcat <$> traverse (generate_case expr) branches


generate_case :: Text -> (Pattern,ExprTree) -> Generator (Text,Text)
generate_case tested_expr (pat,expr) = do
    (decls, e) <- generate_expr expr
    case pat of
        PatLit l -> pure $ (decls,
                            "(_souc_is_equal_integer((" <> tested_expr
                            <> "),(" <> generate_literal l
                            <> "))._souc_bool) ? (" <> e <> ") :\n")
        PatBinding i -> do
            tell $ "union _souc_obj " <> generate_identifier_text i <> ";\n"
            -- the `true` can eventually be replaced by a guard
            pure $ (decls,
                    "(" <> generate_identifier_text i <> " = " <>
                    tested_expr <> ", true) ?\n(" <> e <> ") :\n")


get_next_id :: Generator CIdentifier
get_next_id = do
    n <- get
    put (n+1)
    pure $ CIdentifier $ Text.pack $ 'v' : show n
