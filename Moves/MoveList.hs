{-# LANGUAGE BangPatterns
  #-}

module Moves.MoveList (
    genAMoves, genCMoves
) where

import Control.Monad (when, liftM)
import Control.Monad.ST
import Control.Applicative
import Data.Bits
import Data.Foldable (foldrM)
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed.Mutable as U
import Data.Vector.Generic (unsafeFreeze, unsafeSlice)
import Data.Vector.Unboxed (toList)
import Data.Vector.Algorithms.Insertion (sortByBounds)
import Data.Word

-- import Moves.MoveVector
import Struct.Struct
import Eval.BasicEval
import Moves.Board
import Moves.SEE
import Moves.History

maxMovesPerPos = 256	-- should be fine in almost all postions
maxCaptsPerPos =  64	-- should be fine in almost all postions

type Vect s = U.MVector s Word32			-- move vector

data MList = MList {
                 mlPos    :: MyPos,		-- the position for which we generate the moves
                 mlColor  :: Color,		-- the color for which we generate the moves
                 mlHist   :: PHistory,		-- the frozen history
                 mlDraft  :: Int,		-- the draft, needed for history values
                 mlTTMove :: Maybe Move,	-- TT move (if any): already searched
                 mlKills  :: [Move],		-- list of killer moves
                 mlBads   :: [Move]		-- list of bad captures (with negative SEE)
              }

-- When we generate only the (good) captures:
genCMoves :: MyPos -> Color -> PHistory -> [Move]
genCMoves pos col hist = fst . phaseOnlyCapts $ newCMoveList pos col hist

-- Only for (good) captures
newCMoveList :: MyPos -> Color -> PHistory -> MList
newCMoveList pos col hist
    = MList { mlPos = pos, mlColor = col, mlHist = hist, mlDraft = 0,	-- draft not needed here
              mlTTMove = Nothing, mlKills = [], mlBads = [] }

-- This will also generate only good captures (goodSEE), but will end after that
-- so no need to retain the bad captures
phaseOnlyCapts :: MList -> ([Move], MList)
phaseOnlyCapts ml = runST $ do
    v <- U.new maxCaptsPerPos
    n <- genCapts (mlPos ml) (mlColor ml) v
    if n == 0
       then return ([], ml)
       else do
         usub <- unsafeToCList n v
         let good = filter (goodSEE ml) usub
         return (good, ml)

genCapts :: MyPos -> Color -> Vect s -> ST s Int
genCapts pos col vec
    | null fts  = return 0
    | otherwise = do
          next <- foldrM fromtos 0 fts
          when (next > 1) $ sortVec vec 0 next
          return next
    where fromtos ft@(f, t) i = do
              let !vict = fromIntegral $ 100 - ((pieceVal pos t) `unsafeShiftR` 8)
                  !agrs = fromIntegral $ (pieceVal pos f) `unsafeShiftR` 8
                  Move m = genmv False pos ft
                  !w = m .|. (vict `unsafeShiftL` 24) .|. (agrs `unsafeShiftL` 16)
              U.write vec i m
              return $! i + 1
          fts = genMoveCapt pos col	-- here: transformations are generated separately

-- This replaces the current move generation function
genAMoves :: MyPos -> Color -> PHistory -> Int -> Maybe Move -> [Move] -> [Move]
genAMoves pos col hist d ttmove kills = applyPhases ml phases
    where ml = newMoveList pos col hist d ttmove kills
          phases = [ phaseGoodCapts, phaseKillers, phaseQuiet, phaseBadCapts ]

applyPhases :: MList -> [MList -> ([Move], MList)] -> [Move]
applyPhases = go
    where go _  []       = []
          go ml (ph:phs) = let (l, ml') = ph ml
                           in l ++ go ml' phs

-- When we have a TT move, we put it directly in the first position
-- and it will be delivered without further work
newMoveList :: MyPos -> Color -> PHistory -> Int -> Maybe Move -> [Move] -> MList
newMoveList pos col hist d ttmove kills
    = MList { mlPos = pos, mlColor = col, mlHist = hist, mlDraft = d,
              mlTTMove = ttmove, mlKills = kills, mlBads = [] }

-- A move generation function (here genCapts) takes a vector and a position in it
-- (index, 0 is the first entry), in which it writes
-- the generated moves and returns the next free index in the vector
phaseGoodCapts :: MList -> ([Move], MList)
phaseGoodCapts ml = runST $ do
    v <- U.new maxCaptsPerPos
    n <- genCapts (mlPos ml) (mlColor ml) v
    if n == 0
       then return ([], ml)
       else do
         usub <- unsafeToCList n v
         let (good, bad) = pick usub
         return (good, ml { mlBads = bad })
    where pick = case mlTTMove ml of { Nothing -> go1; Just m -> go2 }
          go1 = partition (goodSEE ml)
          go2 = partition (goodSEE ml) . filter (/= e)
          e   = fromJust $ mlTTMove ml

phaseKillers :: MList -> ([Move], MList)
phaseKillers ml = (mlKills ml, ml)

-- Problem here: how can we sort the quiet moves? We are here in the ST monad,
-- but our history lives in the IO monad. Some "unsafe" call? Freeze?
-- The tests should be done without sort first
phaseQuiet :: MList -> ([Move], MList)
phaseQuiet ml = runST $ do
    v <- U.new maxMovesPerPos
    n <- genQuiet (mlPos ml) (mlColor ml) (mlHist ml) (mlDraft ml) v
    usub <- unsafeToList n v
    return (filter exclTTKs usub, ml)
    where !exclTTKs = case mlTTMove ml of
                         Nothing -> if null (mlKills ml)
                                       then const True
                                       else \e -> if (e `elem` mlKills ml) then False else True
                         Just m  -> if null (mlKills ml)
                                       then \e -> e /= m
                                       else \e -> e /= m && not (e `elem` mlKills ml)

phaseBadCapts :: MList -> ([Move], MList)
phaseBadCapts ml = (mlBads ml, ml)	-- TT move checked earlier

copyWithHist :: Vect s -> PHistory -> Int -> Int -> [Move] -> ST s Int
copyWithHist vec hist d = go
    where go !i []              = return i
          go !i (m@(Move w):ms) = do
             let f = fromSquare m
                 t = toSquare m
                 v = (fromIntegral $ valHistPure hist f t d) .&. 0xFFFF0000
                 !u = w .|. v
             U.unsafeWrite vec i u
             go (i+1) ms

sortVec :: Vect s -> Int -> Int -> ST s ()
sortVec = sortByBounds compare

genQuiet :: MyPos -> Color -> PHistory -> Int -> Vect s -> ST s Int
genQuiet pos col hist d vec = do
    let fts = genMoveCast pos col ++ map (genmv False pos) (genMoveNCapt pos col)
    n <- copyWithHist vec hist d 0 fts
    sortVec vec 0 n
    return n

pieceVal :: MyPos -> Square -> Int
pieceVal p f | Busy _ fig <- tabla p f = matPiece White fig
             | otherwise               = 0

goodSEE :: MList -> Move -> Bool
goodSEE ml move = (fst $ valueSEE p (mlColor ml) to f) >= 0
    where !to = toSquare move
          !p  = mlPos ml
          Busy _ f = tabla p to

unsafeToList :: Int -> Vect s -> ST s [Move]
unsafeToList n mv = do
    v <- unsafeFreeze mv
    let sub = unsafeSlice 0 n v
    return $ map (Move . (.&. 0xFFFF)) $ toList sub	-- take the last 16 bit and make move

unsafeToCList :: Int -> Vect s -> ST s [Move]
unsafeToCList n mv = do
    v <- unsafeFreeze mv
    let sub = unsafeSlice 0 n v
    return $ map (Move . (.&. 0x2FFFF)) $ toList sub	-- take the last 16 bit and make move
                                                        -- but with special (now just hard-coded)
