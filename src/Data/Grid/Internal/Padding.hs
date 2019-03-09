module Data.Grid.Internal.Padding where

-- paddedFromNestedLists :: forall dims a.
--                       Dimensions dims
--                       => a
--                       -> NestedLists dims a
--                       -> (Grid dims a)
-- paddedFromNestedLists padding = tabulate pick
--   where
--     pick :: Coord dims -> a
--     pick  = undefined

-- class Padder (dims :: [Nat]) where
--   pad :: a -> NestedLists dims a -> Grid dims a

-- instance {-# OVERLAPPING #-} KnownNat n => Padder '[n] where
--   pad def xs
--     | length xs <= expectedLength =
--       fromNestedLists' $ xs ++ replicate (expectedLength - length xs) def
--     | otherwise = fromNestedLists' $ xs ++ replicate (length xs - expectedLength) def
--     where
--       expectedLength = fromIntegral.natVal $ Proxy @n

-- instance {-# OVERLAPPABLE #-} (KnownNat x, Padder xs) => Padder (x:xs) where
--   pad def xs
--     | length xs <= expectedLength =
--       fromNestedLists' @'[x] (pad def <$>  xs) -- ++ replicate (expectedLength - length xs) (pad def []))
--     | otherwise = fromNestedLists' $ xs ++ replicate (length xs - expectedLength) def
--     where
--       expectedLength = fromIntegral.natVal $ Proxy @x


