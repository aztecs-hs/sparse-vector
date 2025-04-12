-- |
-- Module      : Data.SparseVector.Mutable
-- Copyright   : (c) Matt Hunzinger, 2025
-- License     : BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  : matt@hunzinger.me
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Data.SparseVector.Mutable
  ( MSparseVector (..),
    empty,
    insert,
    read,
    unsafeRead,
    write,
    unsafeWrite,
    modify,
    unsafeModify,
    toList,
  )
where

import qualified Data.Vector as V
import Data.Vector.Mutable (MVector, PrimMonad (..))
import qualified Data.Vector.Mutable as MV
import Prelude hiding (read)

newtype MSparseVector s a = MSparseVector {unMSparseVector :: MVector s (Maybe a)}

empty :: (PrimMonad m) => m (MSparseVector (PrimState m) a)
empty = do
  vec <- MV.new 0
  return $ MSparseVector vec

insert ::
  (PrimMonad m) =>
  Int ->
  a ->
  MSparseVector (PrimState m) a ->
  m (MSparseVector (PrimState m) a)
insert index a (MSparseVector vec) = do
  let len = MV.length vec
  if len >= index + 1
    then MV.write vec index (Just a) >> return (MSparseVector vec)
    else do
      newVec <- MV.replicate (index + 1) Nothing
      MV.write newVec index (Just a)
      return $ MSparseVector newVec

read :: (PrimMonad m) => MSparseVector (PrimState m) a -> Int -> m (Maybe a)
read (MSparseVector vec) = MV.read vec
{-# INLINE read #-}

unsafeRead :: (PrimMonad m) => MSparseVector (PrimState m) a -> Int -> m (Maybe a)
unsafeRead (MSparseVector vec) = MV.unsafeRead vec
{-# INLINE unsafeRead #-}

write :: (PrimMonad m) => MSparseVector (PrimState m) a -> Int -> Maybe a -> m ()
write (MSparseVector vec) = MV.write vec
{-# INLINE write #-}

unsafeWrite :: (PrimMonad m) => MSparseVector (PrimState m) a -> Int -> Maybe a -> m ()
unsafeWrite (MSparseVector vec) = MV.unsafeWrite vec
{-# INLINE unsafeWrite #-}

modify :: (PrimMonad m) => MSparseVector (PrimState m) a -> Int -> (Maybe a -> Maybe a) -> m ()
modify (MSparseVector vec) index f = do
  val <- MV.read vec index
  MV.write vec index (f val)
{-# INLINE modify #-}

unsafeModify :: (PrimMonad m) => MSparseVector (PrimState m) a -> Int -> ( a ->  a) -> m ()
unsafeModify (MSparseVector vec) index f = do
  val <- MV.unsafeRead vec index
  case val of
    Nothing -> return ()
    Just v -> MV.unsafeWrite vec index (Just (f v))
{-# INLINE unsafeModify #-}

toList :: (PrimMonad m) => MSparseVector (PrimState m) a -> m [Maybe a]
toList (MSparseVector v) = V.toList <$> V.freeze v
{-# INLINE toList #-}
