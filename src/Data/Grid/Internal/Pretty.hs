module Data.Grid.Internal.Pretty where

import Data.List

class PrettyList l where
  prettyList :: l -> String

instance {-# OVERLAPPABLE #-} (Show a) => PrettyList [a] where
  prettyList = show

instance {-# OVERLAPPABLE #-} (Show a) => PrettyList [[a]] where
  prettyList ls = "[" ++ intercalate "\n," (prettyList <$> ls) ++ "]"

instance (Show a) => PrettyList [[[ a ]]] where
  prettyList ls = "[" ++ intercalate "\n\n," (unlines . overRest (" " ++ ) . lines . prettyList <$> ls) ++ "]"
    where
      overRest f (l:ls) = l : fmap f ls
      overRest f ls = ls
