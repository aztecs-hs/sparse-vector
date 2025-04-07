module Data.SparseVector.Mutable where

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
