# Grids

[HACKAGE](http://hackage.haskell.org/package/grids)

**Note** this lib is still pretty new and relatively experimental, as such it
doesn't have great performance characteristics. Maybe don't use it in
performance critical applications.

Grids can have an arbitrary amount of dimensions, specified by a type-level
list of `Nat`s.

Each grid has Functor, Applicative, and Representable instances making it easy
to do **Matlab-style** matrix programming. The `Applicative` instance operates
piecewise making it easy to do most math operations. There's a `Num` instance
for grids holding numbers, so piecewise addition is just `grid1 + grid2`, the
`fromInteger` implementation allows things like `myGrid * 5` to perform
multiplication (although it's likely less efficient than using fmap!)

By combining with `Control.Comonad.Representable.Store` you can do context-wise
**linear transformations** for things like **Image Processing** or **Cellular
Automata**.

All in a typesafe package!

Still working out the best interface for this stuff, feedback is appreciated!

Grids backed by a single contiguous Vector and gain the
associated performance benefits. Currently only boxed immutable vectors are
supported, but let me know if you need other variants.

Here's how we might represent a Tic-Tac-Toe board which we'll fill with
alternating X's and O's:

```haskell
data Piece = X | O deriving Show
toPiece :: Int -> Piece
toPiece n = if even n then X
                      else O

ticTacToe :: Grid [3, 3] Piece
ticTacToe = generate toPiece
```

You can collapse the grid down to nested lists! The output type of
`toNestedLists` depends on your dimensions, e.g.:

- `Grid [3, 3] Piece` will generate: `[[Piece]]`
- `Grid [2, 2, 2] Char` will generate: `[[[Char]]]`
- ...etc

```haskell
λ> toNestedLists ticTacToe
[ [X,O,X]
, [O,X,O]
, [X,O,X]]
```

You can even create a grid from nested lists! `fromNestedLists` returns a grid
if possible, or `Nothing` if the provided lists don't match the structure of
the grid you specify:

```haskell
λ> fromNestedLists [[1, 2], [3, 4]] :: Maybe (Grid '[2, 2] Int)
Just (Grid [[1,2]
           ,[3,4]])
λ> fromNestedLists [[1], [2]] :: Maybe (Grid '[2, 2] Int)
Nothing
```

Grids are Representable Functors, Applicatives, Foldable, and are Traversable!

You can do things like piecewise addition using their applicative instance:

```haskell
λ> let g = generate id :: Grid '[2, 3] Int
λ> g
(Grid [[0,1,2]
      ,[3,4,5]])
λ> liftA2 (+) g g
(Grid [[0,2,4]
      ,[6,8,10]])
λ> liftA2 (*) g g
(Grid [[0,1,4]
      ,[9,16,25]])
```

## Indexing

You can index into a grid using the `Coord` type. The number of
coordinates you need depends on the shape of the grid. 
`Coord` is really just a wrapping over a list of integers. It's recommended that
you use `coord` to safely construct `Coord` values, but you can cheat and use 
the `Coord` constructor or even `OverLoadedLists` if you want to.
 Here's the type of Coord for a few different Grids:

You can get a value out of a grid for a particular index out using `index` from `Data.Functor.Rep`:

```haskell
λ> let g = generate id :: Grid '[2, 3] Int
λ> g
(Grid [[0,1,2]
      ,[3,4,5]])
λ> g `index` Coord [1 , 1]
4
λ> g `index` Coord [1, 0]
3
λ> g `index` Coord [0,  2]
2
```

You can also use the `cell` Lens from `Data.Grid.Lens` to access and mutate
indices:

```haskell
λ> g ^. cell (Coord [0, 1])
1
λ> g & cell (Coord [0, 1]) *~ 1000
(Grid [[0,1000,2],[3,4,5]])
```

## Creation

You can generate a grid by providing a function over the integer position in the grid (`generate`) or by providing
a function over the coordinate position of the cell (`tabulate`). Or of course you can just use `pure`

You can also use the `fromList` and `fromNestedLists` functions which return a
`Maybe (Grid dims a)` depending on whether the input list is well formed.

- `fromList :: [a] -> Maybe (Grid dims a)`
- `fromNestedLists :: NestedLists dims a -> Maybe (Grid dims a)`
- `generate :: (Int -> a) -> Grid dims a`
- `tabulate :: (Coord dims -> a) -> Grid dims a`
- `pure :: a -> Grid dims a`

## Updating

Use either the `cell` lens, or fmap, applicative, traversable.
For batch updates using the underlying Vector implementation use `(//)`

- `(//) :: Grid dims a -> [(Coord dims, a)] -> Grid dims a`
