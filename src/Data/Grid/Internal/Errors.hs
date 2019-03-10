{-# LANGUAGE UndecidableInstances #-}
module Data.Grid.Internal.Errors (type (?!), ErrorMessage(..)) where

import Data.Kind
import GHC.TypeLits

type family (b :: Bool) ?! (e :: ErrorMessage) :: Constraint where
  'True ?! _ = ()
  'False ?! e = TypeError e

infixr 1 ?!
