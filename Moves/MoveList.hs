{-# LANGUAGE BangPatterns
  #-}

module Moves.MoveList (
    genAMoves, genCMoves
) where

import Control.Monad (when, liftM)
import Control.Monad.ST
import Control.Applicative
import Data.Foldable (foldrM)
import Data.List (partition)
import Data.Maybe (fromJust)
import qualified Data.Vector.Unboxed.Mutable as U
import Data.Vector.Generic (unsafeFreeze, unsafeSlice)
import Data.Vector.Unboxed (toList)
import Data.Vector.Algorithms.Insertion (sortByBounds)
import Data.Word

import Moves.MoveVector
import Struct.Struct
import Eval.BasicEval
import Moves.Board
import Moves.SEE

maxMovesPerPos = 256	-- should be fine in almost all postions
maxCaptsPerPos =  64	-- should be fine in almost all postions

type Vect  s = U.MVector s Move			-- move vector
type VectZ s = U.MVector s (Move, Int, Int)	-- zip3 vector (for MVV-LVA sort)

data MList = MList {
                 mlPos    :: MyPos,		-- the position for which we generate the moves
                 mlColor  :: Color,		-- the color for which we generate the moves
                 mlTTMove :: Maybe Move,	-- TT move (if any): already searched
                 mlKills  :: [Move],		-- list of killer moves
                 mlBads   :: [Move]		-- list of bad captures (with negative SEE)
              }

-- When we list moves from a generation phase we take the next move, but have to check it
-- to see if:
-- we already gave that move in an earlier phase (e.g. TT move, or killers)
-- (in case of captures) if it's a good capture
-- Earlier generated moves will be skipped, bad captures will be delayed
data CheckResult = Skip | Delay | Ok

-- When we generate only the (good) captures:
genCMoves :: MyPos -> Color -> [Move]
genCMoves pos col = fst $ phaseOnlyCapts $ newCMoveList pos col

-- Only for (good) captures
newCMoveList :: MyPos -> Color -> MList
newCMoveList pos col
    = MList { mlPos = pos, mlColor = col, mlTTMove = Nothing, mlKills = [], mlBads = [] }

-- This will also generate only good captures (goodSEE), but will end after that
-- so no need to retain the bad captures
phaseOnlyCapts :: MList -> ([Move], MList)
phaseOnlyCapts ml = runST $ do
    v <- U.new maxCaptsPerPos
    n <- genCapts (mlPos ml) (mlColor ml) v
    if n == 0
       then return ([], ml)
       else do
         usub <- unsafeToList n v
         let good = filter (goodSEE ml) usub
         return (good, ml)

genCapts :: MyPos -> Color -> Vect s -> ST s Int
genCapts pos col vec = do
    let fts = genMoveCapt pos col	-- here: transformations are generated separately
    if null fts
        then return 0
        else do
            agrs <- U.new maxCaptsPerPos
            vict <- U.new maxCaptsPerPos
            next <- foldrM (fromtos agrs vict) 0 fts
            when (next > 1) $ sortVecMVVLVA (U.zip3 vec agrs vict) 0 next
            return next
    where fromtos agrs vict ft@(f, t) i = do
              U.write agrs i (pieceVal pos f)
              U.write vict i (pieceVal pos t)
              U.write vec i $ genmv True pos ft
              return $! i + 1

-- This replaces the current move generation function
genAMoves :: MyPos -> Color -> Maybe Move -> [Move] -> [Move]
genAMoves pos col ttmove kills = concat $ snd $ applyPhases ml phases
    where ml = newMoveList pos col ttmove kills
          phases = [ phaseGoodCapts, phaseKillers, phaseQuiet, phaseBadCapts ]

applyPhases :: MList -> [MList -> ([Move], MList)] -> (MList, [[Move]])
applyPhases ml phs = foldr runPhase (ml, []) phs
    where runPhase ph (ml, ls) = (ml', l:ls)
              where (l, ml') = ph ml

-- When we have a TT move, we put it directly in the first position
-- and it will be delivered without further work
newMoveList :: MyPos -> Color -> Maybe Move -> [Move] -> MList
newMoveList pos col ttmove kills
    = MList { mlPos = pos, mlColor = col, mlTTMove = ttmove, mlKills = kills, mlBads = [] }

-- A move generation function (here genCapts) takes a vector and a position in it
-- (index, 0 is the first entry), in which it writes
-- the generated moves and returns the next free index in the vector
phaseGoodCapts :: MList -> ([Move], MList)
phaseGoodCapts ml = runST $ do
    v <- U.new maxMovesPerPos
    n <- genCapts (mlPos ml) (mlColor ml) v
    if n == 0
       then return ([], ml)
       else do
         usub <- unsafeToList n v
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
    n <- genQuiet (mlPos ml) (mlColor ml) v
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

copyList :: Vect s -> Int -> [Move] -> ST s Int
copyList vec i ms = go i ms
    where go !i []     = return i
          go !i (m:ms) = U.unsafeWrite vec i m >> go (i+1) ms

sortVecMVVLVA :: VectZ s -> Int -> Int -> ST s ()
sortVecMVVLVA vec start stop = sortByBounds mvvlva vec start stop
    where mvvlva (_, a1, v1) (_, a2, v2)
              | v1 < v2 = GT
              | v1 > v2 = LT
              | otherwise = compare a1 a2

genQuiet :: MyPos -> Color -> Vect s -> ST s Int
genQuiet pos col vec = do
    let fts = genMoveCast pos col ++ map (genmv False pos) (genMoveNCapt pos col)
    copyList vec 0 fts
    -- Here: sort

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
    return $ toList sub
