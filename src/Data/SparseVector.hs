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
    mapWithKey,

    -- ** Intersection
    intersection,
    intersectionWith,
    intersectionWithKey,

    -- ** Conversion
    fromVector,
    toVector,

    -- ** Mutations
    freeze,
    thaw,
  )
where

import Control.Monad
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
--
-- >>> insert 0 'a' empty
-- SparseVector {unSparseVector = [Just 'a']}
--
-- >>> insert 2 'b' empty
-- SparseVector {unSparseVector = [Nothing,Nothing,Just 'b']}
insert :: Int -> a -> SparseVector a -> SparseVector a
insert index a (SparseVector vec) =
  let len = V.length vec
   in SparseVector $
        if len >= index + 1
          then V.unsafeUpd vec [(index, Just a)]
          else V.snoc (vec V.++ V.replicate (index - len) Nothing) (Just a)
{-# INLINE insert #-}

-- | Lookup an element at a given index in a `SparseVector`.
lookup :: Int -> SparseVector a -> Maybe a
lookup i (SparseVector v) = join $ v V.!? i
{-# INLINE lookup #-}

-- | Delete an index from a `SparseVector`, replacing its cell with @Nothing@.
delete :: Int -> SparseVector a -> SparseVector a
delete index (SparseVector vec) =
  SparseVector $ V.unsafeUpd vec [(index, Nothing)]
{-# INLINE delete #-}

mapWithKey :: (Int -> a -> b) -> SparseVector a -> SparseVector b
mapWithKey f (SparseVector v) =
  let go (i, Just a) = Just $ f i a
      go _ = Nothing
   in SparseVector $ (go <$> V.indexed v)
{-# INLINE mapWithKey #-}

intersection :: SparseVector a -> SparseVector b -> SparseVector a
intersection = intersectionWith $ const
{-# INLINE intersection #-}

intersectionWith :: (a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWith = intersectionWithKey . const
{-# INLINE intersectionWith #-}

intersectionWithKey :: (Int -> a -> b -> c) -> SparseVector a -> SparseVector b -> SparseVector c
intersectionWithKey f a b =
  SparseVector . fmap go . V.indexed . V.zip (unSparseVector a) $ unSparseVector b
  where
    go (i, (Just a', Just b')) = Just $ f i a' b'
    go _ = Nothing
{-# INLINE intersectionWithKey #-}

fromVector :: Vector a -> SparseVector a
fromVector = SparseVector . fmap Just
{-# INLINE fromVector #-}

toVector :: SparseVector a -> Vector a
toVector (SparseVector v) = V.catMaybes v
{-# INLINE toVector #-}

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
