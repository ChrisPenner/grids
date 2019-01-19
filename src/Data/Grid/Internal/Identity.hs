{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Data.Grid.Internal.Identity where

import Data.Grid.Internal.Grid
import Data.Vector as V
import Data.Proxy
import GHC.TypeNats

-- idMatrix
--   :: forall (n :: Nat) (ns :: [Nat]) ind x
--    . (Num x, Dimensions (n : ns), Dimensions ns)
--   => Grid (n : ns) x
-- idMatrix = Grid ns
--  where
--   ns = V.generate (inhabitants @(n : ns)) thing
--   thing n = if n `mod` (inhabitants @ns + 1) == 0 then 1 else 0
