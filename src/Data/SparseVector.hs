{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

-- |
-- Module      : Data.SparseVector
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseVector where

import Data.SparseVector.Mutable (MSparseVector (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (PrimMonad (..))

-- | Sparse n-dimensional vector.
newtype SparseVector a = SparseVector {unSparseVector :: Vector (Maybe a)}
  deriving (Show, Eq, Functor, Foldable, Traversable)

instance Semigroup (SparseVector a) where
  SparseVector v1 <> SparseVector v2 =
    let (lhs, rhs) = if V.length v1 > V.length v2 then (v1, v2) else (v2, v1)
     in SparseVector $ V.update lhs (V.indexed rhs)
  {-# INLINE (<>) #-}

instance Monoid (SparseVector a) where
  mempty = empty
  {-# INLINE mempty #-}

-- | Empty sparse vector.
empty :: SparseVector a
empty = SparseVector V.empty
{-# INLINE empty #-}

-- | Insert an element at a given index into a `SparseVector`.
insert :: Int -> a -> SparseVector a -> SparseVector a
insert index a (SparseVector vec) =
  SparseVector $ case V.length vec >= index + 1 of
    True -> V.unsafeUpd vec [(index, Just a)]
    False -> V.snoc (vec V.++ V.replicate index Nothing) (Just a)
{-# INLINE insert #-}

-- | Lookup an element at a given index in a `SparseVector`.
lookup :: Int -> SparseVector a -> Maybe a
lookup i (SparseVector v) = v V.!? i >>= id
{-# INLINE lookup #-}

-- | Freeze a `MSparseVector` into a `SparseVector`.
freeze :: (PrimMonad m) => MSparseVector (PrimState m) a -> m (SparseVector a)
freeze (MSparseVector vec) = do
  vec' <- V.freeze vec
  return $ SparseVector vec'
{-# INLINE freeze #-}

-- | Unfreeze a `SparseVector` into a `MSparseVector`.
thaw :: (PrimMonad m) => SparseVector a -> m (MSparseVector (PrimState m) a)
thaw (SparseVector vec) = do
  vec' <- V.thaw vec
  return $ MSparseVector vec'
{-# INLINE thaw #-}
