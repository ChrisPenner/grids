# Grids

Grids can have an arbitrary amount of dimensions, specified by a type-level
list of `Nat`s, here's how we might represent a Tic-Tac-Toe board:

```haskell
data Piece = X | O deriving Show
toPiece n = if even n then X
                      else O

ticTacToe :: Grid [3, 3] Piece
ticTacToe = generate toPiece
```

You can collapse the grid down to nested lists! The output type of `toNestedLists` depends on your dimensions, e.g.:

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

You can index into a grid using the `Coord` type family. The number of
coordinates you need depends on the shape of the grid. The Coord is stitched
together using the `:#` constructor from 1 or more `Finite` values. Each Finite
value is scoped to the size of its dimension, so you'll need to prove that each
index is within range (or just use `finite` to wrap an `Integer` and the
compiler will trust you). Here's the type of Coord for a few different Grids:

```haskell
Coord '[1] == Finite 1
Coord '[1, 2] == Finite 1 :# Finite 2
Coord '[1, 2, 3] == Finite 1 :# Finite 2 :# Finite 3
```

You can get a value at an index out using `index` from `Data.Functor.Rep`:

```haskell
λ> let g = generate id :: Grid '[2, 3] Int
λ> g
(Grid [[0,1,2]
      ,[3,4,5]])
λ> g `R.index` (1 :# 1)
4
λ> g `R.index` (1 :# 0)
3
λ> g `R.index` (0 :# 2)
2
```

You can also use the `cell` Lens from `Data.Grid.Lens` to access and mutate
indices:

```haskell
λ> g ^. cell (0 :# 1)
1
λ> g & cell (0 :# 1) *~ 1000
(Grid [[0,1000,2],[3,4,5]])
```



## Creation

You can generate a grid by providing a function over the integer position in the grid (`generate`) or by providing
a function over the coordinate position of the cell (`tabulate`).

You can also use the `fromList` and `fromNestedLists` functions which return a
`Maybe (Grid dims a)` depending on whether the input list is well formed.

- `fromList :: [a] -> Maybe (Grid dims a)`
- `fromNestedLists :: NestedLists dims a -> Maybe (Grid dims a)`
- `generate :: (Int -> a) -> Grid dims a`
- `tabulate :: (Coord dims -> a) -> Grid dims a`

