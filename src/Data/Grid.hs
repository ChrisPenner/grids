module Data.Grid
  (
  -- * Grids
   Grid(..)
   -- * Creation
  , generate
  , Rep.tabulate
  , fromNestedLists
  , fromNestedLists'
  , fromList
  , fromList'

  -- * Collapsing
  , toNestedLists
  , toList

  -- * Indexing
  , Coord(..)
  , coord
  , unconsC
  , appendC
  , Rep.index

  -- * Updating
  , (//)

  -- * Lenses
  , cell

  -- * Convolution
  , autoConvolute
  , safeConvolute
  , safeAutoConvolute
  , convolute
  , gconvolute

  -- ** Window restriction
  , clampWindow
  , wrapWindow

  -- * Permutations
  , transpose
  , permute
  , permuteCoord

  -- * Joining
  , joinGrid
  , splitGrid

  -- * Assorted
  , gridSize

  -- * Typeclasses & Type Families
  , Dimensions
  , NestedLists
  , Neighboring
  , ValidPermutation
  , Permuted

  -- * Re-exports
  , module R
  )
where

import           Data.Grid.Internal.Grid
import           Data.Grid.Internal.Nest
import           Data.Grid.Internal.Lens
import           Data.Grid.Internal.Transpose
import           Data.Grid.Internal.Coord
import           Data.Grid.Internal.Convolution
import           Data.Functor.Rep as R hiding (tabulate, index)
import           Data.Functor.Rep as Rep
