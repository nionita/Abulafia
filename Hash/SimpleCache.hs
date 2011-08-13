{-# LANGUAGE BangPatterns #-}
module Hash.SimpleCache (
    Cache,
    newCache, writeCache, readCache,
    readCacheStats, readCacheBusy
) where
import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Word
import qualified Data.Vector.Storable.Mutable as V
-- import qualified Data.Vector.Unboxed.Mutable as V
import Foreign hiding (new)

import Struct.Struct
import Config.ConfigClass
import Moves.BitBoard (firstOne)

data CacheMV = CMV {
                 busy :: !Int,			-- number of busy entries
                 rtries, wtries, nreads, nwrites, rcolls, wcolls, wrepl :: !Word64
               }
data CacheEn = CacheEn {
                 entkey :: {-# UNPACK #-} !Word64,	-- word 1 (64 bit)
                 entgen :: {-# UNPACK #-} !Word64	-- word 2 (64 bit)
               }

instance Storable CacheEn where
    sizeOf _    = 2 * sizeOf (undefined :: Word64)
    alignment _ = alignment (undefined :: Word64)

    {-# INLINE peek #-}
    peek e = let q = castPtr e
             in do w1 <- peekElemOff q 0
                   w2 <- peekElemOff q 1
                   return $ CacheEn w1 w2

    {-# INLINE poke #-}
    poke e (CacheEn w1 w2)
            = let q = castPtr e
              in do pokeElemOff q 0 w1
                    pokeElemOff q 1 w2
-- type CacheEn = (Word64, Word64)

data Cache = Cache {
                 size :: !Int,			-- capacity (number of entries)
                 maskL, maskH :: !Word64,	-- masks
                 cmv :: MVar CacheMV,		-- variables (like statistics)
                 caens :: V.IOVector CacheEn	-- the entries array
             }

ttMinSize = 512 * 512	-- i.e. 2^18

newCache :: Config c => c -> IO Cache
newCache c = do
    -- Size will be rounded down to a power of 2
    -- and has to be at least 2 ^ 18 entries
    -- i.e. 2 ^ 22 bytes = 4 MB
    let (sz, mL, mH) = sizeAndMasks $ fromIntegral
                                    $ min ttMinSize $ fromMaybe ttMinSize
                                    $ c `getIParam` "ttSize"
        cv = CMV {
                busy = 0, rtries = 0, wtries = 0, nreads = 0,
                nwrites = 0, rcolls = 0, wcolls = 0, wrepl = 0
             }
    xents <- V.new sz
    -- xents <- V.replicate sz ent0
    xcmv <- newMVar cv
    return Cache {
                 size = sz, maskL = mL, maskH = mH,
                 cmv = xcmv, caens = xents
             }
    -- where ent0 = CacheEn 0 0

incrMVar :: Num a => MVar a -> IO ()
incrMVar v = modifyMVar_ v $ \x -> return (x+1)

writeCache :: Cache -> MyPos -> Int -> Int -> Int -> Move -> Int -> IO Bool
writeCache cache pos score tp depth (Move move) nodes = do
    -- cv <- takeMVar (cmv cache)
    cv <- readMVar (cmv cache)
    let idx = zkeyToIdx (zobkey pos) cache
        xw2 = encodeBeDeNo move depth nodes
        xwc = encodeTySc tp score (maskL cache)
        xentry = zkeyToEntry (zobkey pos) xwc (maskH cache) xw2
    V.unsafeWrite (caens cache) idx xentry
    -- Uncomment for statistics:
    -- putMVar (cmv cache) $
    --    cv { wtries = wtries cv + 1, wcolls = wcolls cv + 1,
    --         wrepl = wrepl cv + 1 }
    return True

readCache :: Cache -> MyPos -> IO (Maybe (Int, Int, Int, Move, Int))
readCache cache pos = do
    -- cv <- takeMVar (cmv cache)
    cv <- readMVar (cmv cache)
    let idx = zkeyToIdx (zobkey pos) cache
    xent@(CacheEn xw1 xw2) <- V.unsafeRead (caens cache) idx
    -- xent@(xw1, xw2) <- V.unsafeRead (caens cache) idx
    if xw1 == 0
    -- Uncomment for statistics:
       then do
          -- putMVar (cmv cache) cv { rtries = rtries cv + 1 }
          return Nothing
       else do
          let xzkey = entryToZKey xent idx (maskH cache)
          if xzkey /= zobkey pos
            then do
              -- putMVar (cmv cache) cv { rtries = rtries cv + 1, rcolls = rcolls cv + 1 }
              return Nothing
            else do
              let (ty, sc)  = decodeTySc xw1 (maskL cache)
                  (mv, de, no) = decodeBeDeNo xw2
              -- putMVar (cmv cache) $
              --    cv { rtries = rtries cv + 1, nreads = nreads cv + 1 }
              return $ Just (de, ty, sc, Move mv, no)

{-# INLINE entryToZKey #-}
entryToZKey :: CacheEn -> Int -> Word64 -> ZKey
entryToZKey (CacheEn w1 w2) i m = ((w1 `xor` w2) .&. m) .|. fromIntegral i
-- entryToZKey (w1, w2) i m = ((w1 `xor` w2) .&. m) .|. fromIntegral i

{-# INLINE zkeyToEntry #-}
zkeyToEntry :: ZKey -> Word64 -> Word64 -> Word64 -> CacheEn
zkeyToEntry z c m w2 = CacheEn w1 w2
-- zkeyToEntry z c m w2 = (w1, w2)
    where !w1 = ((z `xor` w2) .&. m) .|. c

{-# INLINE nextGen #-}
nextGen :: Word32 -> Word32
nextGen x = x+1

{-# INLINE zkeyToIdx #-}
zkeyToIdx :: ZKey -> Cache -> Int
zkeyToIdx z ca = fromIntegral $ z .&. maskL ca

{-# INLINE encodeTySc #-}
encodeTySc :: Int -> Int -> Word64 -> Word64
encodeTySc ty sc m = enc
    -- = m .&. fromIntegral (ty .&. 0x3 .|. (sc `shiftL` 2))
    -- = m .&. (tyl .|. fromIntegral scs)
    -- where scs :: Int16
    where scs :: Word32
          !scs = fromIntegral sc .&. 0xFFFF	-- take only 16 bit
          !tyl = fromIntegral ty `shiftL` 16
          !enc = m .&. fromIntegral (tyl .|. scs)

{-# INLINE decodeTySc #-}
decodeTySc :: Word64 -> Word64 -> (Int, Int)
decodeTySc w m = (ty, sc)
    -- where sc  = fromIntegral $ wm .&. 0x3FFFC `shiftR` 2
    --       ty  = fromIntegral $ wm .&. 0x3
    where !sc  = wm .&. 0xFFFF
          !ty  = (wm `shiftR` 16) .&. 0x3
          !wm = fromIntegral $ w .&. m

nodesToNo :: Int -> Word64
nodesToNo = fromIntegral

noToNodes :: Word64 -> Int
noToNodes w  = fromIntegral $ w `shiftR` 24

{-# INLINE encodeBeDeNo #-}
encodeBeDeNo :: Word32 -> Int -> Int -> Word64
encodeBeDeNo be de no
    =     fromIntegral be			-- best move (takes 19 bits)
      .|. ((de' .&. 0x1F) `shiftL` 19)		-- depth on 5 bits (i.e. up to 32)
      .|. (no' `shiftL` 24)			-- nodes on 40 bits
    where !de' = fromIntegral $ min 31 $ de .&. 0x1F
          !no' = nodesToNo no

{-# INLINE decodeBeDeNo #-}
decodeBeDeNo :: Word64 -> (Word32, Int, Int)
decodeBeDeNo w = (be, de, no)
    where !be  = wl .&. 0x7FFFF
          !de  = fromIntegral $ (wl `shiftR` 19) .&. 0x1F
          !wl  = fromIntegral w
          !no  = noToNodes w

readCacheStats :: Cache
               -> IO (Int, Word32, Word64, Word64, Word64, Word64, Word64, Word64, Word64)
readCacheStats c = do
    cv <- readMVar $ cmv c
    return (busy cv, 0, rtries cv, wtries cv, nreads cv,
            nwrites cv, rcolls cv, wcolls cv, wrepl cv)

readCacheBusy :: Cache -> IO Int
readCacheBusy c = do
    cv <- readMVar $ cmv c
    let bum = fromIntegral (busy cv) * (1000 :: Integer)
    return $ fromIntegral $ bum `div` fromIntegral (size c)

ilog :: Int -> Int
ilog x = l - 1
    where pow2s = iterate (* 2) 1
          l = length $ takeWhile (<= x) pow2s

sizeAndMasks :: Int -> (Int, Word64, Word64)
sizeAndMasks isize = (sz, mL, mH)
    where sz = bit $ ilog isize + 1
          mL = fromIntegral $ sz - 1	-- mask for the lower bits
          mH = complement mL		-- mask for the higher bits
