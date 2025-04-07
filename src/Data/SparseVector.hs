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
module Data.SparseVector
  ( -- * Sparse vectors
    SparseVector (..),

    -- ** Operations
    empty,
    insert,
    lookup,
    delete,

    -- ** Intersections
    intersection,
    intersectionWith,
    intersectionWithKey,

    -- ** Mutations
    freeze,
    thaw,
  )
where

import Data.SparseVector.Mutable (MSparseVector (..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Vector.Mutable (PrimMonad (..))
import Prelude hiding (lookup)

-- | Sparse n-dimensional vector.
--
-- A sparse vector is defined as a @Vector (Maybe a)@,
-- where @Maybe a@ is a cell for an element in the sparse vector.
--
-- Inserting elements at some dimension @n@ will grow the vector up to @n@,
-- using @Nothing@ to create empty cells.
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
--
-- Inserting elements at some dimension @n@ will grow the vector up to @n@,
-- using @Nothing@ to create empty cells.
insert :: Int -> a -> SparseVector a -> SparseVector a
insert index a (SparseVector vec) =
  SparseVector $ case V.length vec >= index + 1 of
    True -> V.unsafeUpd vec [(index, Just a)]
    False -> V.snoc (vec V.++ V.replicate (index - 1)  Nothing) (Just a)
{-# INLINE insert #-}

-- | Lookup an element at a given index in a `SparseVector`.
lookup :: Int -> SparseVector a -> Maybe a
lookup i (SparseVector v) = v V.!? i >>= id
{-# INLINE lookup #-}

-- | Delete an index from a `SparseVector`, replacing its cell with @Nothing@.
delete :: Int -> SparseVector a -> SparseVector a
delete index (SparseVector vec) =
  SparseVector $ V.unsafeUpd vec [(index, Nothing)]
{-# INLINE delete #-}

intersection :: SparseVector a -> SparseVector b -> SparseVector a
intersection = intersectionWith $ \a _ -> a

intersectionWith :: (a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWith = intersectionWithKey . const

intersectionWithKey :: (Int -> a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWithKey f a b =
  SparseVector . fmap go . V.indexed . V.zip (unSparseVector a) $ unSparseVector b
  where
    go (i, (Just a', Just b')) = Just $ f i a' b'
    go _ = Nothing

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
