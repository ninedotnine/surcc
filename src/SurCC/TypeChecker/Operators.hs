module SurCC.TypeChecker.Operators (
    in_t,
    ret_t,
    InputType(..),
    ReturnType(..),
    ret,
    ) where

import Prelude hiding (lookup)
import Data.Text (Text)
import Data.Text qualified as Text

import SurCC.Common
import SurCC.TypeChecker.Context

newtype InputType  = InputType SoucType
newtype ReturnType = ReturnType SoucType

in_t :: Text -> InputType
in_t text = InputType (SoucType text (SoucKind 0))

ret_t :: Text -> ReturnType
ret_t text = ReturnType (SoucType text (SoucKind 0))

ret :: (a, ReturnType) -> SoucType
ret (_, ReturnType x) = x
