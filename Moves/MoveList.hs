{-# LANGUAGE BangPatterns
  #-}

module Moves.MoveList (
    genAMoves, genCMoves
) where

import Control.Monad (when, liftM)
import Control.Monad.ST
-- import Control.Monad.Primitive
import Control.Applicative
import Data.Foldable (foldrM)
-- import qualified Data.Vector as U
import qualified Data.Vector.Unboxed.Mutable as U
import Data.Vector.Algorithms.Insertion (sortByBounds)
import Data.Word
import Debug.Trace (trace)

import Moves.MoveVector
import Struct.Struct
import Eval.BasicEval
import Moves.Board
import Moves.SEE

maxMovesPerPos = 256	-- should be fine in almost all postions
maxCaptsPerPos =  64	-- should be fine in almost all postions

type Vect  s = U.MVector s Move			-- move vector
type VectZ s = U.MVector s (Move, Int, Int)	-- zip3 vector (for MVV-LVA sort)

data MList s = MList {
                 mlPos    :: MyPos,		-- the position for which we generate the moves
                 mlColor  :: Color,		-- the color for which we generate the moves
                 mlVec    :: Vect s,		-- vector of moves
                 mlToMove :: !Int,		-- index of the next move to execute
                 mlToGen  :: !Int,		-- index of the next location to generate moves
                 mlNextPh :: GenPhase s,	-- function for the next move generation phase
                 mlCheck  :: CheckFunc s,	-- function to check the next move
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
type CheckFunc s = MList s -> Move -> CheckResult

type GenPhase s = MList s -> ST s (Maybe (MList s))

-- When we generate only the (good) captures:
genCMoves :: MyPos -> Color -> [Move]
genCMoves pos col = runST $ do
    ml <- newCMoveList pos col
    listMoves ml

-- Transforms a move list to a list of moves - lazy
listMoves :: MList s -> ST s [Move]
listMoves ml = do
    sm <- splitMove ml
    case sm of
        -- Just (m, ml') -> (m :) <$> listMoves ml'
        Just (m, ml') -> do
            rest <- listMoves ml'
            return $ m : rest
        Nothing       -> return []

-- Only for (good) captures
newCMoveList :: MyPos -> Color -> ST s (MList s)
newCMoveList pos col = do
    v <- U.new maxCaptsPerPos
    return MList { mlPos = pos, mlColor = col, mlVec = v, mlToMove = 0, mlToGen = 0,
                   mlNextPh = nextPhaseOnlyCapts,
                   mlCheck = constOk, mlTTMove = Nothing, mlKills = [], mlBads = [] }

-- This will also generate only good captures (checkSEE), but will end after that
nextPhaseOnlyCapts :: GenPhase s
nextPhaseOnlyCapts ml = do
    n <- genCapts (mlPos ml) (mlColor ml) (mlVec ml) (mlToGen ml)
    return $ Just ml { mlToGen = n, mlNextPh = nextPhaseEnd, mlCheck = checkSEE }

nextPhaseEnd :: GenPhase s
nextPhaseEnd _ = return Nothing

constOk :: CheckFunc s
constOk _ _ = Ok

-- Split the first move from the move list and return it together with
-- the new move list (without the first move). Return Nothing if there
-- is no further move
splitMove :: MList s -> ST s (Maybe (Move, MList s))
splitMove ml
    | mlToMove ml >= mlToGen ml = do
        mml <- trace trm $ nextPhase ml
        case mml of
            Nothing  -> return Nothing
            Just ml' -> splitMove ml'
    | otherwise = do
        m <- U.unsafeRead (mlVec ml) (mlToMove ml)
        case mlCheck ml ml m of
            Ok    -> return $ Just (m, ml1)
            Skip  -> splitMove ml1
            Delay -> splitMove ml1 { mlBads = m : mlBads ml }
    -- where !ml1 = ml { mlToMove = mlToMove ml + 1 }
    where ml1 = ml { mlToMove = mlToMove ml + 1 }
          trm  = show (mlToMove ml) ++ " >= " ++ show (mlToGen ml) ++ " : next phase"

genCapts :: MyPos -> Color -> Vect s -> Int -> ST s Int
genCapts pos col vec start = do
    let fts = genMoveCapt pos col	-- here: transformations are generated separately
    if null fts
        then return start
        else do
            agrs <- U.new maxCaptsPerPos
            vict <- U.new maxCaptsPerPos
            next <- foldrM (fromtos agrs vict) start fts
            when (next > start + 1) $ sortVecMVVLVA (U.zip3 vec agrs vict) start next
            return next
    where fromtos agrs vict ft@(f, t) i = do
              U.write agrs i (pieceVal pos f)
              U.write vict i (pieceVal pos t)
              U.write vec i $ genmv True pos ft
              return $! i + 1

pieceVal :: MyPos -> Square -> Int
pieceVal p f | Busy _ fig <- tabla p f = matPiece White fig
             | otherwise               = 0

-- This replaces the current move generation function
genAMoves :: MyPos -> Color -> Maybe Move -> [Move] -> [Move]
genAMoves pos col ttmove kills = runST $ do
    ml <- newMoveList pos col ttmove kills
    listMoves ml

-- When we have a TT move, we put it directly in the first position
-- and it will be delivered without further work
newMoveList :: MyPos -> Color -> Maybe Move -> [Move] -> ST s (MList s)
newMoveList pos col ttmove kills = do
    v <- U.new maxMovesPerPos
    return ml { mlVec = v, mlTTMove = ttmove }
    where !ml = MList { mlPos = pos, mlColor = col, mlToMove = 0, mlToGen = 0,
                        mlNextPh = nextPhaseGoodCapts, mlCheck = constOk,
                        mlTTMove = Nothing, mlKills = kills, mlBads = [] }

-- A move generation function (here genCapts) takes a vector and a position in it
-- (index, 0 is the first entry), in which it writes
-- the generated moves and returns the next free index in the vector
nextPhaseGoodCapts :: GenPhase s
nextPhaseGoodCapts ml = do
    n <- genCapts (mlPos ml) (mlColor ml) (mlVec ml) (mlToGen ml)
    return $ Just ml { mlToGen = n, mlNextPh = nextPhaseKillers, mlCheck = exclTTMove }
    where exclTTMove = case mlTTMove ml of
                           Nothing -> checkSEE
                           Just m  -> \_ e -> if e == m then Skip else checkSEE ml e
                           -- Nothing -> constOk
                           -- Just m  -> \_ e -> if e == m then Skip else constOk ml e

nextPhaseKillers :: GenPhase s
nextPhaseKillers ml = do
    n <- copyList (mlVec ml) (mlToGen ml) (mlKills ml)
    return $ Just ml { mlToGen = n, mlNextPh = nextPhaseQuiet, mlCheck = exclTTMove }
    where exclTTMove = case mlTTMove ml of
                           Nothing -> constOk
                           Just m  -> \_ e -> if e == m then Skip else Ok

-- Problem here: how can we sort the quiet moves? We are here in the ST monad,
-- but our history lives in the IO monad. Some "unsafe" call? Freeze?
-- The tests should be done without sort first
nextPhaseQuiet :: GenPhase s
nextPhaseQuiet ml = do
    n <- genQuiet (mlPos ml) (mlColor ml) (mlVec ml) (mlToGen ml)
    return $ Just ml { mlToGen = n, mlNextPh = nextPhaseBadCapts, mlCheck = exclTTKs }
    where exclTTKs = case mlTTMove ml of
                         Nothing -> if null (mlKills ml)
                                       then constOk
                                       else \_ e -> if (e `elem` mlKills ml) then Skip else Ok
                         Just m  -> if null (mlKills ml)
                                       then \_ e -> if (e == m) then Skip else Ok
                                       else \_ e -> if (e == m || e `elem` mlKills ml)
                                                       then Skip
                                                       else Ok

nextPhaseBadCapts :: GenPhase s
nextPhaseBadCapts ml = do
    -- n <- copyList (mlVec ml) (mlToGen ml) (reverse $ mlBads ml)
    n <- copyList (mlVec ml) (mlToGen ml) (mlBads ml)
    return $ Just ml { mlToGen = n, mlNextPh = nextPhaseEnd, mlCheck = constOk }	-- TT move checked earlier

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

genQuiet :: MyPos -> Color -> Vect s -> Int -> ST s Int
genQuiet pos col vec start = do
    let fts = genMoveCast pos col ++ map (genmv False pos) (genMoveNCapt pos col)
    copyList vec start fts

checkSEE :: CheckFunc s
checkSEE ml move = if (fst $ valueSEE p (mlColor ml) to f) >= 0 then Ok else Delay
    where !to = toSquare move
          !p  = mlPos ml
          Busy _ f = tabla p to

-- Invoke the next phase of move generation and return new move list if
-- there really was one, otherwise returns Nothing, which should be interpreted
-- as the move list is done
-- nextPhase :: GenPhase s
nextPhase ml = (mlNextPh ml) ml
