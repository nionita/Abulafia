module TransTab where

import Foreign.Marshall.Array
import Foreign.Storable
import Foreign.Ptr

type Index = Int
type ZKey = Word64		-- comes from struct
type Mask = Word64

cacheLineSize = 64	-- this should be the size in bytes of a memory cache line on modern processors

-- The data type Cell and its Storable instance is declared only for alignement purposes
-- The operations in the cell are done on PCacheEn elements
data Cell

instance Storable Cell where
    sizeOf _    = cacheLineSize
    alignment _ = cacheLineSize
    peek _      = return undefined
    poke _ _    = return ()

data TTable
    = TTable {
          mem 	 	 :: !Ptr Cell,	-- the cache line aligned byte array for the data
          lomask, himask :: !Mask	-- the masks depending on the size (in entries) of the table
      }

data PCacheEn = PCacheEn { lo, hi :: {-# UNPACK #-} !Word64 }	-- a packed TT entry
pCacheEnSize = 2 * sizeOf (0 :: Word64)

instance Storable PCacheEn where ...	-- here: the Storable methods for CacheEn

{--
A packed cache entry consists of 2 Word64 parts (the order of the bit fields is fixed):
- word 1 contains (ttbitlen is the number of bits to represent the table length, i.e. for 2^20 entries, ttbitlen = 20):
	- 2 parts of the ZKey:
		- the first 64 - ttbitlen - 2 higher bits of the ZKey
		- the last 2 bits of the ZKey
	- generation part 1 - variable length depending on number of tt entries (= ttbitlen - 16)
	- score - 16 bits
- word 2 contains:
	- node type - 1 bit: exact = 1, or not exact = 0
	- depth -  5 bits
	- nodes - 32 bits
	- node type - 1 bit: lower = 1, or upper limit
	- move  - 19 bits
	- generation part 2 - 6 bits
It results that anyway the number of entries in the table must be at least 2^16 (in which case part 1 of the generation
is empty (0 bits).
In this implementation only the part 2 (6 bit) of the generation will be used
--}

data CacheEn = CacheEn {
                   partZKey  :: !ZKey,		-- partial ZKey first part, not covered by index
                   partZCell :: !Word32,	-- partial ZKey third part, corresponding to cell index
                   posType   :: !Word32,	-- position type: 0 - upper limit, 1 - lower limit, 2 - exact score
                   posScore  :: !Int,		-- score stored actually on 16 (signed) bits
                   posDepth  :: !Word32,	-- search depth stored on 5 bits (max depth = 31)
                   posBest   :: !Word32,	-- best move (19? bits)
                   posNodes  :: !Word32		-- nodes searched under this position (are 32 unsigned bits enough?)
               }

part3Mask = 0x03		-- the cell has 4 entries (other option: 8)
cellMask = complement part3Mask	-- for speed we keep both masks
minEntries = 2 ^ 16

-- Create a new transposition table with a given number of entries
-- The given number will be rounded up to the next power of 2
newTTable :: Int -> IO TTable
newTTable nentries = do
    memc <- mallocArray pow2q
    return TTable { mem = memc, lomask = lom, himask = complement lom }
    where !pow2 = max minEntries $ apropriatePowOf2 nentries
          !lom  = pow2 - 1
          !pow2q = pow2 `shiftR` 2

-- This computes the beginning index where the given entry should be stored
-- and the index of the cell containing that entry
-- The (low) mask of the transposition table is also used - this determines the size of the index
zKeyToCellIndex :: TTable -> ZKey -> (Ptr PCacheEn, Index)
zKeyToCellIndex tt zkey = (base, idx)
    where !idx = zkey .&. lomask tt
          !cell = idx .&. cellMask
          !base = mem tt `plusPtr` cell * pCacheEnSize

-- Retrieve the ZKey of a packed entry
getZKey :: TTable -> Index -> PCacheEn -> ZKey
getZKey tt idx (PCacheEn {w1 = lo}) = zkey
    where !zkey =  lo  .&. himask tt			-- this is the first part of the stored ZKey
               .|. idx .&. lomask tt .&. cellMask	-- this is the second (index dependent) part of the stored ZKey
               .|. lo  .&. part3Mask			-- this is the third part (2 or 3 bits) of the stored ZKey

-- Given a ZKey, an index and a packed cache entry, determine if that entry has the same ZKey
isSameEntry :: TTable -> ZKey -> Index -> PCacheEn -> Bool
isSameEntry tt zkey idx pCE = zkey == getZKey tt idx pCE

-- Search a position in table based on ZKey
-- The position ZKey determines the cell where the TT entry should be, and there we do a linear search
-- (i.e. 4 comparisons in case of a miss)
retrieveEntry :: TTable -> ZKey -> IO (Maybe PCacheEn)
retrieveEntry tt zkey = do
    let (crt, idx) = zKeyToCellIndex tt zkey
    retrieve tt zkey idx crt 4

retrieve :: TTable -> ZKey -> Index -> Ptr PCacheEn -> Int -> IO (Maybe PCacheEn)
retrieve tt zkey idx crt tries = do
    pCE <- peek crt
    if isSameEntry tt zkey idx pCE
       then return (Just pCE)
       else if tries <= 1
               then return Nothing
               else do
                   let !crt1 = crt `plusPtr` pCacheEnSize
                       !tries1 = tries - 1
                   retrieve tt zkey idx crt1 tries1

-- Write the position in the table
-- We want to keep table entries that:
-- + are from the same generation, or
-- + have been searched deeper, or
-- + have a more precise score (node type 2 before 1 and 0)
-- Why not to consider the number of nodes searched under the position? This says how much effort is put
-- behind that position and could be also interesting
-- So actually we always search in the whole cell in the hope to find the zkey and replace it
-- but also keep track of the weakest entry in the cell, which will be replaced otherwise
storeEntry :: TTable -> ZKey -> PCacheEn -> Int -> IO ()
storeEntry tt zkey pCE gen = do
    let (crt, idx) = zKeyToCellIndex tt zkey
    store tt zkey pCE idx gen crt crt 4

store :: TTable -> ZKey -> PCacheEn -> Index -> Int -> Ptr PCacheEn -> Ptr PCacheEn -> Int -> IO ()
store tt zkey pCE idx gen crt rep tries = do
    if isSamEntry tt zkey idx pCE
       then poke pCE crt	-- here we found the same entry: just update
       else if tries <= 1
               then poke pCE rep	-- replace the weakest entry with the current one
               else do	-- search further
                   rep1 <- chooseReplaceEntry gen crt rep
                   let !crt1 = crt `plusPtr` pCacheEnSize
                       !tries1 = tries - 1
                   store tt zkey idx crt1 rep1 tries1

-- Here we implement the logic which decides if which entry is weaker
-- If the current entry has the current generation then we consider the old replacement to be weaker
-- without to consider other criteria in case it has itself the current generation
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

packCacheEn :: ...
unpackCacheEn :: Mask -> Mask -> PCacheEn -> Index ->