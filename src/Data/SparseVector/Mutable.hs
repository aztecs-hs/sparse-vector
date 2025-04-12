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
    write,
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

write :: (PrimMonad m) => MSparseVector (PrimState m) a -> Int -> Maybe a -> m ()
write (MSparseVector vec) = MV.write vec

toList :: (PrimMonad m) => MSparseVector (PrimState m) a -> m [Maybe a]
toList (MSparseVector v) = V.toList <$> V.freeze v
{-# INLINE toList #-}
