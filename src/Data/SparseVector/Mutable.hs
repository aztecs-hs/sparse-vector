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
    insert,
  )
where

import Data.Vector.Mutable (MVector, PrimMonad (..))
import qualified Data.Vector.Mutable as MV

newtype MSparseVector s a = MSparseVector {unMSparseVector :: MVector s (Maybe a)}

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
