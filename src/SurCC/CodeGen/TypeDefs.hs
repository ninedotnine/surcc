module SurCC.CodeGen.TypeDefs (
    gen_typedefs
) where

import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Function
import Data.List (groupBy)
import Data.Text (Text)

import TextShow (showt)

import SurCC.Common
import SurCC.CodeGen.Common ()
import SurCC.CodeGen.ExprGen (gen_identifier)

gen_typedefs :: [Bound] -> Text
gen_typedefs = group_ids <&> number <&> map textify <&> fold
    where
        -- FIXME groupBy might not work if list is unsorted
        group_ids :: [Bound] -> [[Identifier]]
        group_ids = groupBy types_eq <&> map (map get_id)
            where
                get_id (Bound i _) = i
                types_eq :: Bound -> Bound -> Bool
                types_eq (Bound _ t0) (Bound _ t1) = t0 == t1

        number :: [[Identifier]] -> [(Identifier,Word)]
        number = map (enumerated 0) <&> fold
            where
                enumerated :: Word -> [Identifier] -> [(Identifier,Word)]
                enumerated n = \case
                    (x:xs) -> (x,n) : enumerated (n+1) xs
                    _ -> []

        textify :: (Identifier,Word) -> Text
        textify (i,n) = "const union _souc_obj " <> gen_identifier i
                        <> " = {._souc_int = " <> showt n <> "};\n"
