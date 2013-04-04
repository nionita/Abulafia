{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE EmptyDataDecls #-}
module Hash.TransTab (
    Cache, newCache, readCache, writeCache, newGener,
    checkProp
    ) where

import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Int
import Data.Word
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Test.QuickCheck hiding ((.&.))

import Config.Config
import Config.ConfigClass
import Struct.Struct

type Index = Int
type Mask = Word64

cacheLineSize :: Int
cacheLineSize = 64	-- this should be the size in bytes of a memory cache line on modern processors

-- The data type Cell and its Storable instance is declared only for alignement purposes
-- The operations in the cell are done on PCacheEn elements
data Cell

instance Storable Cell where
    sizeOf _    = cacheLineSize
    alignment _ = cacheLineSize
    peek _      = return undefined
    poke _ _    = return ()

data Cache
    = Cache {
          mem 	:: Ptr Cell,	-- the cache line aligned byte array for the data
          lomask, mimask,
          himask :: !Mask,	-- masks depending on the size (in entries) of the table
          gener :: !Word64	-- the generation of the current search
      }

data PCacheEn = PCacheEn { lo, hi :: {-# UNPACK #-} !Word64 }	-- a packed TT entry

pCacheEnSize :: Int
pCacheEnSize  = 2 * sizeOf (undefined :: Word64)	-- i.e. 16 bytes

instance Storable PCacheEn where
    sizeOf _    = pCacheEnSize
    alignment _ = alignment  (undefined :: Word64)
    {-# INLINE peek #-}
    peek e = let q = castPtr e
             in do w1 <- peekElemOff q 0
                   w2 <- peekElemOff q 1
                   return $ PCacheEn { lo = w1, hi = w2 }
    {-# INLINE poke #-}
    poke e (PCacheEn { lo = w1, hi = w2 })
           = let q = castPtr e
             in do pokeElemOff q 0 w1
                   pokeElemOff q 1 w2

{--
A packed cache entry consists of 2 Word64 parts (the order of the bit fields is fixed):
- word 1 (high) contains (ttbitlen is the number of bits to represent the table length in cells,
  i.e. for 2^18 cells, ttbitlen = 18):
	- part 1 of the ZKey: - the first (64 - ttbitlen - 2) higher bits of the ZKey
	- unused bits - variable length depending on number of tt entries (= ttbitlen - 16)
	- score - 16 bits
	- part 3 of the ZKey: the last 2 bits
- word 2 (low) contains:
	- nodes - 32 bits
	- node type - 2 bit: exact = 2, lower = 1, upper = 0
	- depth -  5 bits
	- move  - 19 bits
	- generation - 6 bits
It results that anyway the number of entries in the table must be at least 2^18
(i.e. 2^16 cells with 4 entries each), in which case the "unused bits" part is empty (0 bits).
Part 2 of the ZKey is the cell number where the entry resides.

These are fields of the word 1 and the masks that we keep (here for minimum of 2^18 entries):
|6   5    2         1         0|
|32109...1098765432109876543210|
|<--part 1--><----score-----><>|
|<--himask--><-----lomask----->|    lomask and mimask cover also the unused bits, if any,
|...      ...<-----mimask--->..|    and the himask is shorter in that case
--}

part3Mask :: Mask
part3Mask = 0x03 :: Mask	-- the cell has 4 entries (other option: 8)

minEntries :: Int
minEntries = 2 ^ 18

-- Create a new transposition table with a given number of entries
-- The given number will be rounded up to the next power of 2
newCache :: Config c => c -> IO Cache
newCache c = do
    let nentries = max minEntries $ nextPowOf2 $ fromMaybe minEntries $ c `getIParam` "ttSize"
        ncells   = nentries `div` 4	-- 4 entries per cell
        lom      = fromIntegral $ nentries - 1
        mim      = lom .&. cellMask
    memc <- mallocArray ncells
    return Cache { mem = memc, lomask = lom, mimask = mim, himask = complement lom, gener = 0 }
    where cellMask = complement part3Mask	-- for speed we keep both masks

-- Increase the generation by 1 for a new search, wrap on 6 bits (i.e. 0 to 63)
newGener :: Cache -> Cache
newGener c = c { gener = (gener c + 1) .&. 0x3F }

-- This computes the adress of the first entry of the cell where an entry given by the key
-- should be stored, and the (ideal) index of that entry
-- The (low) mask of the transposition table is also used - this determines the size of the index
zKeyToCellIndex :: Cache -> ZKey -> (Ptr PCacheEn, Index)
zKeyToCellIndex tt zkey = (base, idx)
    where idx = fromIntegral $ zkey .&. lomask tt
          -- This is the wanted calculation:
          -- cell = idx `unsafeShiftR` 2				-- 2 because we have 4 entries per cell
          -- base = mem tt `plusPtr` (cell * sizeOf Cell)
          -- NB: plusPtr advances Bytes!
          -- And this is how it is done efficiently:
          -- idx is entry number, we want cell number: 4 entries per cell ==> shiftR 2
          -- plusPtr needs bytes, 16 bytes/entry * 4 entries/cell = 64 bytes/cell ==> shiftL 6
          !base = mem tt `plusPtr` ((idx `unsafeShiftR` 2) `unsafeShiftL` 6)

-- Retrieve the ZKey of a packed entry
getZKey :: Cache -> Index -> PCacheEn -> ZKey
getZKey tt idx (PCacheEn {hi = w1}) = zkey
    where !zkey =  w1 .&. himask tt	-- the first part of the stored ZKey
               .|. widx .&. mimask tt	-- the second part of the stored ZKey
               .|. w1 .&. part3Mask	-- the 3rd part of stored ZKey
          widx = fromIntegral idx

-- Given a ZKey, an index and a packed cache entry, determine if that entry has the same ZKey
isSameEntry :: Cache -> ZKey -> Index -> PCacheEn -> Bool
isSameEntry tt zkey idx pCE = zkey == getZKey tt idx pCE

-- Search a position in table based on ZKey
-- The position ZKey determines the cell where the TT entry should be, and there we do a linear search
-- (i.e. 4 comparisons in case of a miss)
readCache :: Cache -> ZKey -> IO (Maybe (Int, Int, Int, Move, Int))
readCache tt zkey = do
    mpce <- retrieveEntry tt zkey
    return $! fmap cacheEnToQuint mpce

retrieveEntry :: Cache -> ZKey -> IO (Maybe PCacheEn)
retrieveEntry tt zkey = do
    let (bas, idx) = zKeyToCellIndex tt zkey
    retrieve idx bas (4::Int)
    where retrieve idx = go
              where go !crt0 !tries0 = do
                       pCE <- peek crt0
                       if isSameEntry tt zkey idx pCE
                          then return (Just pCE)
                          else if tries0 <= 1
                                  then return Nothing
                                  else do
                                      let crt1 = crt0 `plusPtr` pCacheEnSize
                                          tries1 = tries0 - 1
                                      go crt1 tries1

-- Write the position in the table
-- We want to keep table entries that:
-- + are from the same generation, or
-- + have more nodes behind (from a previous search), or
-- + have been searched deeper, or
-- + have a more precise score (node type 2 before 1 and 0)
-- That's why we choose the order in second word like it is (easy comparison)
-- Actually we always search in the whole cell in the hope to find the zkey and replace it
-- but also keep track of the weakest entry in the cell, which will be replaced otherwise
writeCache :: Cache -> ZKey -> Int -> Int -> Int -> Move -> Int -> IO ()
writeCache tt zkey depth tp score move nodes = do
    let (bas, idx) = zKeyToCellIndex tt zkey
        gen = gener tt
        pCE = quintToCacheEn tt zkey depth tp score move nodes
    store gen pCE idx bas bas (4::Int)
    where store gen pCE idx = go
              where go !crt0 !rep0 !tries0 = do
                       cpCE <- peek crt0
                       if isSameEntry tt zkey idx cpCE
                          then poke crt0 pCE	 -- here we found the same entry: just update
                          else if tries0 <= 1
                              then poke rep0 pCE -- replace the weakest entry with the current one
                              else do	-- search further
                                  rep1 <- chooseReplaceEntry gen crt0 rep0
                                  let crt1 = crt0 `plusPtr` pCacheEnSize
                                      tries1 = tries0 - 1
                                  go crt1 rep1 tries1

-- Here we implement the logic which decides which entry is weaker
-- If the current entry has the current generation then we consider the old replacement to be weaker
-- without to consider other criteria in case it has itself the current generation
chooseReplaceEntry :: Word64 -> Ptr PCacheEn -> Ptr PCacheEn -> IO (Ptr PCacheEn)
chooseReplaceEntry gen crt rep = if rep == crt then return rep else do
    crte <- peek crt
    if generation crte == gen
       then return rep
       else do
           repe <- peek rep
           if betterpart repe > betterpart crte
              then return crt
              else return rep
    where generation = (.&. 0x3F) . lo
          betterpart = lo	-- there is some noise at the end of that word (26 bits), but we don't care

quintToCacheEn :: Cache -> ZKey -> Int -> Int -> Int -> Move -> Int -> PCacheEn
quintToCacheEn tt zkey depth tp score (Move move) nodes = pCE
    where w1 =   (zkey .&. himask tt)
             .|. fromIntegral ((score .&. 0xFFFF) `unsafeShiftL` 2)
             .|. (zkey .&. part3Mask)
          w2 =   (fromIntegral nodes `unsafeShiftL` 32)
             .|. (fromIntegral tp    `unsafeShiftL` 30)
             .|. (fromIntegral depth `unsafeShiftL` 25)
             .|. (fromIntegral move  `unsafeShiftL`  6)
             .|. gener tt
          !pCE = PCacheEn { hi = w1, lo = w2 }

cacheEnToQuint :: PCacheEn -> (Int, Int, Int, Move, Int)
cacheEnToQuint (PCacheEn { hi = w1, lo = w2 }) = (de, ty, sc, Move mv, no)
    where scp = (w1 .&. 0x3FFFF) `unsafeShiftR` 2
          ssc = (fromIntegral scp) :: Int16
          !sc = fromIntegral ssc
          !no = fromIntegral $ w2 `unsafeShiftR` 32
          -- w2low = (fromIntegral (w2 .&. 0xFFFFFFFF)) :: Word32
          w2low = (fromIntegral w2) :: Word32	-- does it work so?
          w21 = w2low `unsafeShiftR` 6		-- don't need the generation
          !mv = fromIntegral $ w21 .&. 0x7FFFF
          w22 = w21 `unsafeShiftR` 19
          !de = fromIntegral $ w22 .&. 0x1F
          !ty = fromIntegral $ w22 `unsafeShiftR` 5
          -- perhaps is not a good idea to make them dependent on each other
          -- this must be tested and optimised for speed

nextPowOf2 :: Int -> Int
nextPowOf2 x = bit (l - 1)
    where pow2s = iterate (* 2) 1
          l = length $ takeWhile (<= x) pow2s

----------- QuickCheck -------------
newtype Quint = Q (Int, Int, Int, Move, Int) deriving Show

mvm = (1 `shiftL` 19) - 1 :: Word32

instance Arbitrary Quint where
    arbitrary = do
        sc <- choose (-20000, 20000)
        ty <- choose (0, 2)
        de <- choose (0, 31)
        mv <- arbitrary `suchThat` (<= mvm)
        no <- arbitrary `suchThat` (>= 0)
        return $ Q (de, ty, sc, Move mv, no)

{--
newtype Gener = G Int
instance Arbitrary Gener where
     arbitrary = do
        g <- arbitrary `suchThat` (inRange (0, 256))
        return $ G g
--}

prop_Inverse :: Cache -> ZKey -> Int -> Quint -> Bool
prop_Inverse tt zkey _ (Q q@(de, ty, sc, mv, no))	-- unused: gen
    = q == cacheEnToQuint (quintToCacheEn tt zkey de ty sc mv no)

checkProp :: IO ()
checkProp = do
    tt <- newCache defaultConfig
    let zkey = 0
        gen  = 0 :: Int
    putStrLn $ "Fix zkey & gen: " ++ show zkey ++ ", " ++ show gen
    -- quickCheck $ prop_Inverse tt zkey gen
    verboseCheck $ prop_Inverse tt zkey gen
    putStrLn $ "Arbitrary zkey, fixed gen = " ++ show gen
    -- quickCheck $ \z -> prop_Inverse tt z gen
    verboseCheck $ \z -> prop_Inverse tt z gen
{--
    putStrLn $ "Arbitrary gen, fixed zkey = " ++ show gen
    -- quickCheck $ \g -> prop_Inverse tt zkey g
    verboseCheck $ \(G g) -> do let tt' = head $ drop g (iterate newGener tt)
                                return $ prop_Inverse tt zkey g
--}
