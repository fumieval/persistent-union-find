{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module PersistentUnionFind where

import PersistentVector as P
import qualified Data.Vector as V
import Text.Dot qualified as Dot
import Control.Monad

type Key = Int

data UnionFind = UnionFind
  { ranks :: !(P.PArray Int)
  , parents :: !(P.PArray Int)
  } deriving Show

toDot :: UnionFind -> Dot.Dot ()
toDot UnionFind{..} = do
  nodes <- forM (toVector ranks) $ \rank -> Dot.node [("label", show rank)]
  forM_ (V.indexed (toVector parents)) $ \(i, parent) -> do
    Dot.edge (nodes V.! i) (nodes V.! parent) []

new :: Int -> UnionFind
new len = UnionFind
  { ranks = P.fromVector $ V.replicate len 0
  , parents = P.fromVector $ V.generate len id
  }

union :: UnionFind -> Key -> Key -> UnionFind
union uf0 (find uf0 -> (uf1, i)) (find uf1 -> (uf2, j))
  | i == j = uf2
  | otherwise = case compare p q of
    LT -> uf2 { parents = set i j (parents uf2) }
    EQ -> uf2 { parents = set j i (parents uf2), ranks = set i (p + 1) (ranks uf2) }
    GT -> uf2 { parents = set j i (parents uf2) }
  where
    p = ranks uf2 P.! i
    q = ranks uf2 P.! j

find :: UnionFind -> Key -> (UnionFind, Key)
find uf key = case parents uf P.! key of
  parent
    | key == parent -> (uf, parent)
    | (uf1, result) <- find uf parent ->
      ( uf1 {parents = set key result (parents uf)} -- path compression
      , result)

