{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
module PersistentVector where

import Prelude hiding (read)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Data.IORef
import System.IO.Unsafe

newtype PArray a = PArray { unPArray :: IORef (PArrayData a) }

instance Show a => Show (PArray a) where
  showsPrec d = showsPrec d . toVector

data PArrayData a = Base (MV.IOVector a)
  | Diff !Int !a (PArray a)

displayPArray :: Show a => PArray a -> IO ()
displayPArray (PArray ref) = readIORef ref >>= go where
  go (Base vec) = V.freeze vec >>= print
  go (Diff i v r) = print (i, v) >> displayPArray r

new :: V.Vector a -> IO (PArray a)
new vec = V.thaw vec >>= fmap PArray . newIORef . Base

read :: PArray a -> Int -> IO a
read (PArray ref) i = readIORef ref >>= \case
  Base vec -> MV.read vec i
  Diff j v inner
    | i == j -> pure v
    | otherwise -> read inner i

(!) :: PArray a -> Int -> a
array ! i = unsafePerformIO $ read array i

set :: Int -> a -> PArray a -> PArray a
set i v ref = unsafePerformIO $ do
  vec <- reroot ref
  old <- MV.read vec i
  MV.write vec i v
  ref' <- newIORef (Base vec)
  writeIORef (unPArray ref) $ Diff i old (PArray ref')
  pure (PArray ref')

reroot :: PArray a -> IO (MV.IOVector a)
reroot ref = readIORef (unPArray ref) >>= \case
  Base vec -> pure vec
  Diff i v inner -> do
    vec <- reroot inner
    old <- MV.read vec i
    MV.write vec i v
    writeIORef (unPArray ref) (Base vec)
    writeIORef (unPArray inner) (Diff i old ref)
    pure vec

fromVector :: V.Vector a -> PArray a
fromVector = unsafePerformIO . new

toVector :: PArray a -> V.Vector a
toVector v = unsafePerformIO $ reroot v >>= V.freeze

testdata :: PArray Int
testdata = fromVector $ V.fromList [0, 1, 2, 3, 4, 5]