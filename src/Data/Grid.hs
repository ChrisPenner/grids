{-# language DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# language ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Grid where

import Data.Distributive
import Data.Functor.Rep
import qualified Data.Vector as V
import GHC.TypeLits
import Data.Proxy
import Data.Functor.Compose
import Control.Lens

newtype Grid (width :: Nat) (height :: Nat) a =
  Grid (Compose V.Vector V.Vector a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance (KnownNat w, KnownNat h) => Applicative (Grid w h) where
  pure = tabulate . const
  (Grid (Compose a)) <*> (Grid (Compose b)) = Grid . Compose $ V.zipWith (V.zipWith ($)) a b

instance (KnownNat w, KnownNat h) => Distributive (Grid w h) where
  distribute = distributeRep

instance (KnownNat w, KnownNat h) => Representable (Grid w h) where
  type Rep (Grid w h) = (Int, Int)
  index (Grid (Compose spc)) (x, y) = spc V.! x V.! y
  tabulate f = Grid . Compose $ V.generate width (\ x -> V.generate height (\y -> f (x, y)))
    where
      width = fromIntegral $ natVal @w Proxy
      height = fromIntegral $ natVal @h Proxy

instance (KnownNat w, KnownNat h) => FunctorWithIndex (Int, Int) (Grid w h) where
  imap = imapRep

instance (KnownNat w, KnownNat h) => FoldableWithIndex (Int, Int) (Grid w h) where
  ifoldMap = ifoldMapRep

instance (KnownNat w, KnownNat h) => TraversableWithIndex (Int, Int) (Grid w h) where
  itraverse = itraverseRep
