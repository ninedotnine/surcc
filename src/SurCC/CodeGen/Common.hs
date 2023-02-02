{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SurCC.CodeGen.Common (
    CIdentifier(..),
    Generator,
    Decls(..),
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.Set (Set)
import Data.String (IsString)
import Data.Text (Text)

import SurCC.Common (Identifier)

type Generator a = WriterT Text (State Word) a

newtype CIdentifier = CIdentifier Text
                   deriving (Eq, Ord, IsString)


newtype Decls = Decls (Set Identifier)
    deriving (Eq, Ord, Show, Semigroup, Monoid)
