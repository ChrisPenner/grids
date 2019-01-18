{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
module Data.Grid.Internal.Identity where

import Data.Grid.Internal.Types
import Data.Vector as V
import Data.Proxy

idMatrix
  :: forall n ns ind x
   . (Num x, Dimensions (n : ns), Dimensions ns)
  => Grid ind (n : ns) x
idMatrix = Grid ns
 where
  ns = V.generate (gridSize (Proxy @(n : ns))) thing
  thing n = if n `mod` (gridSize (Proxy @ns) + 1) == 0 then 1 else 0
