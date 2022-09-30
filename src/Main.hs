module Main (main) where

import Control.Monad
import PersistentUnionFind
import Text.Dot qualified as Dot
import System.Random

main :: IO ()
main = do
  -- dat <- map (map read . words) . lines <$> getContents
  -- let size = succ $ maximum $ concat dat :: Int
  let size = 100
  dat <- replicateM 100 $ do
    i <- randomRIO (0, size - 2)
    j <- randomRIO (i + 1, size - 1)
    pure (i, j)
  go (new size) dat
  where
    go !uf ((p, q) : dat) = go (union uf p q) dat
    go uf [] = putStrLn $ Dot.showDot $ toDot uf