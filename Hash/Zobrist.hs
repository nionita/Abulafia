module Hash.Zobrist (
    ZKey,
    zobrist,
    zobMove,
    zobPiece,
    zobCastKw, zobCastQw, zobCastKb, zobCastQb,
    zobEP
) where

import Data.Array.IArray
import Data.Array.Unboxed
import Data.Bits
import Data.Word
import System.Random
import Foreign.Storable
import Control.Exception (assert)

import Struct.Struct

-- type ZKey = Word64

{-# INLINE hashMe #-}
hashMe :: BasicPos -> ZKey
-- hashMe p = black p `xor` slide p `xor` kkrq p `xor` diag p `xor` epcas p
-- hashMe p = bpblack p `xor` (bpslide p `rotateL` 3)
--              `xor` (bpkkrq p `rotateL` 7)
--              `xor` (bpdiag p `rotateL` 13)
--              `xor` (bpepcas p `rotateL` 29)
hashMe p = bpblack p `xor` (bpslide p `rotateL` 13)
             `xor` (bpkkrq p `rotateL` 19)
             `xor` (bpdiag p `rotateL` 43)
             `xor` (bpepcas p `rotateL` 59)

-- genInit = 2*3*5*7*11*13 + 1   -- 1210
genInit = 118863
zLen = 781

zobrist :: UArray Int ZKey
zobrist = array (0, zLen-1) $ take zLen $ zip [0..] randomW64s

w32Tow64 :: [Word64] -> [Word64]
w32Tow64 (x:y:ws) = w : w32Tow64 ws
    where w = (x `shift` 32) .|. y

randomInts :: [Int]
randomInts = randoms (mkStdGen genInit)

randomW64s :: [ZKey]
randomW64s = toW64 $ map fromIntegral randomInts
    where isize = sizeOf (undefined :: Word)
          toW64 = case isize of
                    64 -> id
                    _  -> w32Tow64

-- When black is moving: xor with that number
zobMove :: ZKey
zobMove = fromIntegral $ zobrist!(12*64)

-- For every pice type of every color on every valid
-- field: one index in zobrist (0 to 12*64-1)
zobPiece :: Color -> Piece -> Square -> ZKey
zobPiece c p sq = zobrist!idx
    where p2int = if c == White then p2intw else p2intb
          idx = (p2int!p) .|. sq

p2intw, p2intb :: UArray Piece Int
p2intw = array (Pawn, King) $ zip [Pawn .. King] [0, 64 .. ]
p2intb = array (Pawn, King) $ zip [Pawn .. King] [b0, b1 .. ]
    where b0 = p2intw!King + 64
          b1 = b0 + 64

zobCastBegin = 12*64+1
zobCastKw = zobrist!zobCastBegin
zobCastQw = zobrist!(zobCastBegin + 1)
zobCastKb = zobrist!(zobCastBegin + 2)
zobCastQb = zobrist!(zobCastBegin + 3)

zobEP :: Int -> ZKey
zobEP x = assert (x >= 1 && x <= 8) $ zobrist!(zobCastBegin + 3 + x)
