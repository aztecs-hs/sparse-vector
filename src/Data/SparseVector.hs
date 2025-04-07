module Data.SparseVector where

import Data.SparseVector.Mutable (MSparseVector (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (PrimMonad (..))

newtype SparseVector a = SparseVector {unSparseVector :: Vector (Maybe a)}
  deriving (Show, Eq)

empty :: SparseVector a
empty = SparseVector V.empty

insert :: Int -> a -> SparseVector a -> SparseVector a
insert index a (SparseVector vec) =
  SparseVector $ case V.length vec >= index + 1 of
    True -> V.unsafeUpd vec [(index, Just a)]
    False -> V.snoc (vec V.++ V.replicate index Nothing) (Just a)

freeze :: (PrimMonad m) => MSparseVector (PrimState m) a -> m (SparseVector a)
freeze (MSparseVector vec) = do
  vec' <- V.freeze vec
  return $ SparseVector vec'

thaw :: (PrimMonad m) => SparseVector a -> m (MSparseVector (PrimState m) a)
thaw (SparseVector vec) = do
  vec' <- V.thaw vec
  return $ MSparseVector vec'
