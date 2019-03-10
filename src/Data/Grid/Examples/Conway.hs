{-|
This module walks through implementing
<https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life Conway's Game of Live>
using @grids@. Read this module from top to bottom
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Grid.Examples.Conway where

import Data.Grid
import Data.Foldable
import Data.List
import Data.Functor.Compose

{-|
Conway's game of life is a form of <https://en.wikipedia.org/wiki/Cellular_automaton Cellular Automata>;
This means that it's a system of cells where a simulation is described in terms
of the __neighbourhood__ of a cell.
We'll implement the standard set of rules for Conway's Game of Life
using convolution with @grids@ (which you can read about on wikipedia)!

@grids@ provides the 'convolute' and 'autoConvolute' helpers to allow performing
neighbourhood-aware computations without too much trouble.

'autoConvolute' allows you to provide a window-size via a Type Application, a function
for adapting the coordinates of the neighbours, which is useful for providing
behaviour for cases where coordinates are out of bounds. Lastly we provide
a function which can reduce the provided neighbourhood of a cell back down to
a single cell which will be slotted into the final structure.

With these things in mind, we write our 'step' function which performs a single
simulation step on a 'Grid' using a window-reduction rule that we'll write next!

> step :: (IsGrid dims) => Grid dims Bool -> Grid dims Bool
> step = autoConvolute @[3, 3] wrapBounds rule

We use the 'wrapBounds' strategy for handling out-of-bounds indices which will
occur when trying to get the neighbourhood of cells along the edge of the grid.
'wrapBounds' means we'll grab the next cell from the opposite side of the grid,
wrapping around Pacman style.
-}
step :: (IsGrid dims) => Grid dims Bool -> Grid dims Bool
step = autoConvolute @[3, 3] wrapBounds rule

{-|
Next up we'll write the reduction rule itself. We've already decided to use a [3, 3]
neighbourhood in the previous step by using a Type Application, GHC could infer
it in thi case if we didn't provide it; but I find it helpful to be as clear
as possible.

So the goal is to go from a [3, 3] neighbourhood of a cell down to a single cell
again; the liveness of each cell is a 'Bool' which is 'True' if the cell is
alive, 'False' otherwise.

Take a look at how we do it:

> rule :: Grid [3, 3] Bool -> Bool
> rule window' =
>   (currentCellAlive && livingNeighbours == 2) || livingNeighbours == 3
>   where
>     (currentCellAlive, neighbours) = partitionFocus window'
>     livingNeighbours = length . filter id . toList . Compose $ neighbours

We'll start with the @where@ clauses.

Firstly we use the nifty 'partitionFocus' combinator which separates out the
center of a window from the surrounding neighbours as a tuple. Intuitively, We name
these parts @currentCellAlive@ and @neighbours@. Next we count how many neighbours
are actually alive by leaning on the 'Foldable' typeclass. 'partitionFocus' returns
the neighbours as a @Grid [3, 3] (Maybe Bool)@ in this case; so by wrapping it with
'Compose' we can fold over only the 'Just' values, e.g. the neighbours! We then
filter out the 'False' ones with @filter id@ and we know how many neighbours are
alive!

Back to the main definition, all that's left is to write the actual logic of the rule; there are many ways
to write in the rule; but I hope you trust I've picked a good one :D

That's it! By combining our rule with the powers of auto-convolution we've written
the core of Conway's game of life in just a dozen lines of code. Clever us!

Keep reading and we'll add some helpers so we can actually try running it.
-}
rule :: Grid [3, 3] Bool -> Bool
rule window' =
  (currentCellAlive && livingNeighbours == 2) || livingNeighbours == 3
  where
    (currentCellAlive, neighbours) = partitionFocus window'
    livingNeighbours = length . filter id . toList . Compose $ neighbours

{-|
If we're going to simulate a game we need to have a starting position!

Let's start off with a glider hanging out in the top left corner.

Since we'll be using a 2 dimensional grid
we can draw out our grid as a list of lists (i.e. @[String]@) and use
'fromNestedLists'' to build it into a 'Grid' for us! Note that the trailing @'@
denotes the unsafe version of 'fromNestedLists' which will error if we give
it input that mismatches our expected dimensions, use the safe version unless
you're confident about your input sources!

> start :: Grid '[10, 10] Bool
> start = (== '#') <$> fromNestedLists'
>   [ ".#........"
>   , "..#......."
>   , "###......."
>   , ".........."
>   , ".........."
>   , ".........."
>   , ".........."
>   , ".........."
>   , ".........."
>   , ".........."
>   ]
-}
start :: Grid '[10, 10] Bool
start = (== '#') <$> fromNestedLists'
  [ ".#........"
  , "..#......."
  , "###......."
  , ".........."
  , ".........."
  , ".........."
  , ".........."
  , ".........."
  , ".........."
  , ".........."
  ]


{-|
Now that we've got a good starting point we can run a few steps of our simulation
and see where we end up!

'step' is an Endomorphisn, i.e. a function from a type to the same type, so
we can use 'iterate' to generate an infinite list of steps where each subsequent
step is equal to applying the 'step' function on the previous iteration.

By using '!!' we can index into the infinite list and see what things look like
after a set number of iterations.

> simulate :: Int -> Grid '[10, 10] Bool
> simulate i = iterate step start !! i
-}

simulate :: Int -> Grid '[10, 10] Bool
simulate i = iterate step start !! i


{-|
The built-in show instance for 'Grid' isn't perfect, so let's write a nice
one for our simulation. We can extract our grid as a list of lists of @Bool@, i.e. @[[Bool]]@;
and collapse it into a string so we can print it to the console.

> showGrid :: (IsGrid '[x, y]) => Grid '[x, y] Bool -> String
> showGrid = intercalate "\n" . toNestedLists . fmap showBool
>   where
>     showBool :: Bool -> Char
>     showBool True = '#'
>     showBool False = '.'

Let's how far our glider can make it to after 10 iterations

> λ> putStrLn . showGrid $ simulate 10
> ..........
> ..........
> ..........
> ....#.....
> ..#.#.....
> ...##.....
> ..........
> ..........
> ..........
> ..........

That's it for this guide! Thanks!

-}
showGrid :: (IsGrid '[x, y]) => Grid '[x, y] Bool -> String
showGrid = intercalate "\n" . toNestedLists . fmap showBool
  where
    showBool :: Bool -> Char
    showBool True = '#'
    showBool False = '.'
