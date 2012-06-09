{-# LANGUAGE BangPatterns
           , TypeFamilies
           , MultiParamTypeClasses
           , RankNTypes
  #-}

module Moves.MoveVector (
) where

import Control.Monad (when, liftM)
import Control.Monad.ST.Lazy
import Control.Monad.Primitive
import Control.Applicative
import Data.Foldable (foldrM)
import qualified Data.Vector.Primitive as P
import           Data.Vector.Generic         (Vector)	-- class
import qualified Data.Vector.Generic         as G
import           Data.Vector.Generic.Mutable (MVector)	-- class
import qualified Data.Vector.Generic.Mutable as M
import           Data.Vector.Unboxed.Mutable (STVector)
import qualified Data.Vector.Unboxed         as UB
import qualified Data.Vector.Unboxed.Mutable as U
import Data.Vector.Algorithms.Insertion (sortByBounds)
import Data.Word

import Struct.Struct

newtype instance U.MVector s Move = MV_Move (P.MVector s Word32)
newtype instance UB.Vector   Move = V_Move  (P.Vector    Word32)

instance U.Unbox Move

instance MVector U.MVector Move where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    {-# INLINE basicClear #-}
    {-# INLINE basicSet #-}
    {-# INLINE basicUnsafeCopy #-}
    {-# INLINE basicUnsafeGrow #-}
    basicLength (MV_Move v) = M.basicLength v
    basicUnsafeSlice i n (MV_Move v) = MV_Move $ M.basicUnsafeSlice i n v
    basicOverlaps (MV_Move v1) (MV_Move v2) = M.basicOverlaps v1 v2
    basicUnsafeNew n = MV_Move `liftM` M.basicUnsafeNew n
    basicUnsafeReplicate n (Move x) = MV_Move `liftM` M.basicUnsafeReplicate n x
    basicUnsafeRead (MV_Move v) i = Move `liftM` M.basicUnsafeRead v i
    basicUnsafeWrite (MV_Move v) i (Move x) = M.basicUnsafeWrite v i x
    basicClear (MV_Move v) = M.basicClear v
    basicSet (MV_Move v) (Move x) = M.basicSet v x
    basicUnsafeCopy (MV_Move v1) (MV_Move v2) = M.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_Move v1) (MV_Move v2) = M.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_Move v) n = MV_Move `liftM` M.basicUnsafeGrow v n

instance Vector UB.Vector Move where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq #-}
    basicUnsafeFreeze (MV_Move v) = V_Move `liftM` G.basicUnsafeFreeze v
    basicUnsafeThaw (V_Move v) = MV_Move `liftM` G.basicUnsafeThaw v
    basicLength (V_Move v) = G.basicLength v
    basicUnsafeSlice i n (V_Move v) = V_Move $ G.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_Move v) i = Move `liftM` G.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_Move mv) (V_Move v) = G.basicUnsafeCopy mv v
    elemseq _ = seq
