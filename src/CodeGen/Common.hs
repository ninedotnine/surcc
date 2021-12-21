{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module CodeGen.Common (
    CIdentifier(..),
    Generator,
) where

import Control.Monad.State
import Control.Monad.Writer
import Data.String (IsString)
import Data.Text (Text)

type Generator a = WriterT Text (State Word) a

newtype CIdentifier = CIdentifier Text
                   deriving (Eq, Ord, IsString)
