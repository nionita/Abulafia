{-# LANGUAGE BangPatterns #-}
module Moves.BitBoard (
    popCount, popCount1, lsb, bbToSquares, less, firstOne, exactOne, bbToSquaresBB
) where

-- import Control.Exception (assert)
import Data.Array.Base
import Data.Array.Unboxed
import Data.Bits hiding (popCount)
import qualified Data.Bits as B
import Data.List.Stream (unfoldr)
import Data.Word

import Struct.Struct

-- First, the bit scan funtion
-- This could be replaced through an asm function for CPUs which have bitscan
{-# INLINE lsb #-}
lsb :: BBoard -> BBoard
lsb b = b .&. (-b)

-- {-# INLINE exactOne #-}
exactOne :: BBoard -> Bool
-- exactOne b = b /= 0 && b .&. complement (lsb b) == 0
exactOne !b = b /= 0 && b `less` lsb b == 0

{-# INLINE less #-}
less :: BBoard -> BBoard -> BBoard
less w1 w2 = w1 .&. complement w2

{-# INLINE firstOne #-}
firstOne :: BBoard -> Square
firstOne b = case b of
                 b1 -> case fromIntegral $ (lsb b1 * bitScanMagic) `shiftR` 58 of
                           x -> bitScanDatabase `unsafeAt` x

bitScanMagic :: BBoard
bitScanMagic = 0x07EDD5E59A4E28C2

bitScanDatabase :: UArray Int Int
bitScanDatabase = array (0, 63) paar
    where ones = take 64 $ zip [0..] $ iterate (`shiftL` 1) 1
          paar = [(mbsm bit, i) | (i, bit) <- ones]
          mbsm x = fromIntegral $ (x * bitScanMagic) `shiftR` 58

-- Population count function, good for bigger populations:
-- Already optimized!
popCount :: BBoard -> Int
{--
popCount bb = fromIntegral $ case  bb         - ((bb `shiftR` 1)  .&. k1) of { x1 ->
                             case (x1 .&. k2) + ((x1 `shiftR` 2)  .&. k2) of { x2 ->
                             case (x2         +  (x2 `shiftR` 4)) .&. k4  of { x3 ->
                             case (x3 * kf) `shiftR` 56 of { x4 -> x4 }}}}

k1, k2, k4, kf :: Word64
k1 = 0x5555555555555555
k2 = 0x3333333333333333
k4 = 0x0F0F0F0F0F0F0F0F
kf = 0x0101010101010101
--}
popCount = B.popCount

-- Population count function, good for small populations:
-- Already optimized!
popCount1 :: BBoard -> Int
{--
popCount1 bb = go bb 0
    where go 0  !n = n
          go !x !n = let !n1 = n + 1
                         !x1 = x `xor` lsb x
                     in go x1 n1
--}
popCount1 = B.popCount

{-# INLINE bbToSquares #-}
bbToSquares :: BBoard -> [Square]
bbToSquares bb = unfoldr f bb
    where f :: BBoard -> Maybe (Square, BBoard)
          f 0 = Nothing
          -- f b = case firstOne b of sq -> Just (sq, b `clearBit` sq)
          f b = case firstOne b of sq -> case b `clearBit` sq of b1 -> Just (sq, b1)

{-# INLINE bbToSquaresBB #-}
bbToSquaresBB :: (Square -> BBoard) -> BBoard -> BBoard
bbToSquaresBB f bb = go bb 0
    where go 0  w = w
          go !b w = let !sq = firstOne b
                        !b1 = b `clearBit` sq
                        !w1 = f sq .|. w
                    in go b1 w1
