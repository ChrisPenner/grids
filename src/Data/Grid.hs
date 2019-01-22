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
  , convolute

  -- ** Window restriction
  , clampWindow
  , wrapWindow
  , safeWindow

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
  )
where

import           Data.Grid.Internal.Grid
import           Data.Grid.Internal.Nest
import           Data.Grid.Internal.Lens
import           Data.Grid.Internal.Transpose
import           Data.Grid.Internal.Coord
import           Data.Grid.Internal.Convolution
import           Data.Functor.Rep as Rep
