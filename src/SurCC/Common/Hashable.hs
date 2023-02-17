{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module SurCC.Common.Hashable (Hashable) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import SurCC.Common

deriving instance Generic Identifier
instance Hashable Identifier

deriving instance Generic SoucKind
instance Hashable SoucKind

deriving instance Generic SoucType
instance Hashable SoucType

deriving instance Generic TypeVar
instance Hashable TypeVar
