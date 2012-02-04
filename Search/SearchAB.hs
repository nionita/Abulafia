-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Search.SearchAB (
    alphaBeta, logmes
) where

import Control.Monad
import Data.Bits ((.&.))
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Data.Array.Base
import Data.Array.Unboxed
import Data.Maybe (fromMaybe)

import Search.SearchMonad
import Search.AlbetaTypes
import Struct.Struct
import Struct.Status hiding (stats)
import Moves.BaseTypes
import Moves.Base
import Search.TreeState

debug = False

-- Parameter for aspiration
useAspirWin = True

-- Some fix search parameter
timeNodes   = 1024 - 1	-- check time every so many nodes
scoreGrain  = 4	-- score granularity
pathGrain   = fromIntegral scoreGrain	-- path Granularity
depthForCM  = 7 -- from this depth inform current move
minToStore  = 1 -- minimum remaining depth to store the position in hash
minToRetr   = 1 -- minimum remaining depth to retrieve
maxDepthExt = 3 -- maximum depth extension
useNegHist  = False	-- when not cutting - negative history
negHistMNo  = 1		-- how many moves get negative history

-- Parameters for late move reduction:
lmrActive   = True
lmrMinDFRes = 8		-- minimum depth for full research when failed high in null window
lmrMinDRed  = 2		-- minimum reduced depth
lmrMaxDepth = 15
lmrMaxWidth = 63
lmrPv     = 13
lmrRest   = 8
-- LMR parameter optimisation (lmrPv, lmrRest):
-- lm1 = 2, 1	-> elo -127 +- 58
-- lm2 = 3, 2	-> elo  -14 +- 52
-- lm3 = 5, 3	-> elo   17 +- 55
-- lm4 = 8, 5	-> elo   32 +- 53
-- lm5 = 13, 8	-> elo   92 +- 54 --> this is it
lmrReducePv, lmrReduceArr :: UArray (Int, Int) Int
lmrReducePv  = array ((1, 1), (lmrMaxDepth, lmrMaxWidth))
    [((i, j), ceiling $ logrd i j lmrPv) | i <- [1..lmrMaxDepth], j <- [1..lmrMaxWidth]]
lmrReduceArr = array ((1, 1), (lmrMaxDepth, lmrMaxWidth))
    [((i, j), ceiling $ logrd i j lmrRest) | i <- [1..lmrMaxDepth], j <- [1..lmrMaxWidth]]

logrd :: Int -> Int -> Double -> Double
logrd i j f = 1 + log (fromIntegral i) * log (fromIntegral j) / f

-- Parameters for futility pruning:
futilActive = True
maxFutilDepth = 3
futilMargins :: UArray Int Int
futilMargins = array (1, 3) [ (1, 325), (2, 550), (3, 900) ]	-- F1
-- futilMargins = array (1, 3) [ (1, 125), (2, 350), (3, 500) ]	-- F2
-- futilMargins = array (1, 3) [ (1, 75), (2, 150), (3, 300) ]	-- F3

-- Parameters for quiescent search:
qsBetaCut  = True	-- use beta cut in QS?
qsDeltaCut = True	-- use delta prune in QS?
qsMaxChess = 2		-- max number of chess for a quiet search path

-- Parameters for null move pruning
nulActivate = True		-- activate null move reduction
nulRedux    = 3 -- depth reduction for null move
nulMoves    = 2	-- how many null moves in sequence are allowed (one or two)
nulMargin, nulSubmrg :: Int
nulMargin   = 1		-- margin to search the null move (over beta)
nulSubmrg   = 10	-- improved margin
nulSubAct   = True

-- Parameters for internal iterative deepening
useIID      = True
minIIDPV    = 5
minIIDCut   = 7
maxIIDDepth = 4
iidNewDepth = subtract 1
-- iidNewDepth = `shiftR` 1	-- i.e. div 2

-- Parameter for quiescenst search
inEndlessCheck, qsDelta :: Int
inEndlessCheck = -scoreGrain	-- there is a risk to be left in check
qsDelta     = 1100

type Search m a = forall r. STPlus r PVState m a

alpha0, beta0 :: Int
alpha0 = minBound + 2000
beta0  = maxBound - 2000

data Pvsl = Pvsl {
        pvPath :: Path,		-- pv path
        pvNodes :: !Int,	-- number of nodes in the current search
        pvGood  :: !Bool	-- beta cut or alpha improvement
    } deriving Show

data Killer = NoKiller | OneKiller Move Int | TwoKillers Move Int Move Int
                         deriving Show

-- Read only parameters of the search, so that we can change them programatically
data PVReadOnly
    = PVReadOnly {
          school :: !Bool,	-- running in learning mode
          albest :: !Bool,	-- always choose the best move (i.e. first)
          timeli :: !Bool,	-- do we have time limit?
          abmili :: !Int	-- abort when after this milisecond
    } deriving Show

data NodeType = PVNode | CutNode | AllNode deriving (Eq, Show)

deepNodeType PVNode  = PVNode
deepNodeType CutNode = AllNode
deepNodeType AllNode = CutNode

nextNodeType PVNode = CutNode
nextNodeType t      = t

newtype Alt e = Alt { unalt :: [e] } deriving Show
newtype Seq e = Seq { unseq :: [e] } deriving Show

pvro00 = PVReadOnly { school = False, albest = False, timeli = False, abmili = 0 }

alphaBeta :: Node m => ABControl -> m (Int, [Move], [Move])
alphaBeta abc = {-# SCC "alphaBeta" #-} do
    let !d = maxdepth abc
        rmvs = Alt $ rootmvs abc
        lpv  = Seq $ lastpv abc
        searchReduced a b = pvRootSearch a      b     d lpv rmvs True
        searchLow       b = pvRootSearch alpha0 b     d lpv rmvs True
        searchHigh    a   = pvRootSearch a      beta0 d lpv rmvs True
        searchFull        = pvRootSearch alpha0 beta0 d lpv rmvs False	-- ???
        pvro = PVReadOnly { school = learnev abc, albest = best abc,
                            timeli = stoptime abc /= 0, abmili = stoptime abc }
        -- pvs0 = if learnev abc then pvsInit { ronly = pvro1 } else pvsInit
        pvs0 = pvsInit { ronly = pvro } :: PVState
    r <- if useAspirWin
         then case lastscore abc of
             Just sp -> do
                let !alpha1 = sp - window abc
                    !beta1  = sp + window abc
                -- informStr $ "+++ Aspi search with d = " ++ show d
                --                ++ " alpha = " ++ show alpha1
                --                ++ " beta = " ++ show beta1
                -- aspirWin alpha1 beta1 d lpv rmvs aspTries
                r1@((s1, es1, _), pvsf)
                    <- {-# SCC "alphaBetaSearchReduced" #-}
                         runSearch (searchReduced alpha1 beta1) pvs0
                if abort pvsf || (s1 > alpha1 && s1 < beta1 && not (nullSeq es1))
                    then return r1
                    else {-# SCC "alphaBetaSearchFullRe" #-} runSearch searchFull pvs0
             Nothing -> {-# SCC "alphaBetaSearchFullIn" #-} runSearch searchFull pvs0
         else {-# SCC "alphaBetaSearchFull" #-} runSearch searchFull pvs0
    -- when aborted, return the last found good move
    -- we have to trust that abort is never done in draft 1!
    if abort (snd r)
       then return (fromMaybe 0 $ lastscore abc, lastpv abc, [])
       else return $! case fst r of (s, Seq path, Alt rmvs) -> (s, path, rmvs)

pvslToPair :: Pvsl -> (Int, [Move])
pvslToPair (Pvsl { pvPath = p }) = (score, pv)
    where pv = unseq $ pathMoves p
          de = pathDepth p
          sc = pathScore p
          score = scoreToExtern sc de

pvslToMove :: Pvsl -> Move
pvslToMove (Pvsl { pvPath = Path { pathMoves = Seq (m:_)}}) = m

-- The internal score is for weird for found mates (always mate)
-- Turn it to nicer score by considering path lenght to mate
scoreToExtern :: Int -> Int -> Int
scoreToExtern sc de
    | nearmate sc = if sc > 0 then sc - de else sc + de
    | otherwise   = sc

insertToPvs :: Node m => Int -> Pvsl -> [Pvsl] -> Search m [Pvsl]
insertToPvs _ p [] = return [p]
insertToPvs d p ps@(q:qs)
    | d == 1 && (betters || equals) = return $ p : ps
    | pmate && not qmate            = return $ p : ps
    | not pmate && qmate            = do ir <- insertToPvs d p qs
                                         return $ q : ir
    | pmate && betters              = return $ p : ps
    | bettern || equaln && betters  = return $ p : ps
    | otherwise                    = do ir <- insertToPvs d p qs
                                        return $ q : ir
    where betters = pvPath p >  pvPath q
          equals  = pvPath p == pvPath q
          equaln  = pvNodes p == pvNodes q
          bettern = pvNodes p > pvNodes q
          pmate   = pnearmate $ pvPath p
          qmate   = pnearmate $ pvPath q

indentActive :: Node m => String -> Search m ()
indentActive s = do
    ad <- getLevelState >>= level
    lift $ informStr $ take ad (repeat ' ') ++ s

indentPassive :: Node m => String -> Search m ()
indentPassive _ = return ()

pindent, qindent :: Node m => String -> Search m ()
pindent = indentPassive
qindent = indentPassive

bestFirst :: Eq e => [e] -> [e] -> ([e], [e]) -> [e]
bestFirst path kl (es1, es2)
    | null path = es1 ++ kl ++ delall es2 kl
    | otherwise = e : delete e es1 ++ kl ++ delall es2 (e : kl)
    where delall = foldr delete
          e = head path

pushKiller :: Move -> Int -> Killer -> Killer
pushKiller !e s NoKiller = OneKiller e s
pushKiller !e s ok@(OneKiller e1 s1)
    = if e == e1
         then ok
         else TwoKillers e s e1 s1
pushKiller !e s tk@(TwoKillers e1 s1 e2 s2)
    | e == e1 || e == e2 = tk
    | otherwise          = TwoKillers e s e1 s1

killerToList :: Killer -> [Move]
killerToList NoKiller = []
killerToList (OneKiller e _) = [e]
killerToList (TwoKillers e1 _ e2 _) = [e1, e2]

moreThanOne :: [e] -> Bool
moreThanOne (_:_:_) = True
moreThanOne _       = False

--- Communication to the outside - some convenience functions ---

informBM a b c d = inform (BestMv a b c d)

informCM a b = inform (CurrMv a b)

informStr s = inform (InfoStr s)

logmes s = inform (LogMes s)

informBest :: Node m => Int -> Int -> [Move] -> Search m ()
informBest s d es = do
    n <- lift curNodes
    lift $ informBM s d n es

-- First: alpha/beta
-- Here is the new search

data TreeVars = TreeVars {
         draft :: Int,		-- draft of the search, readonly
         stans :: Int,		-- statistic: total node count
         staqs :: Int		-- statistic: total nodes in qsearch
     }

data LevelVars = LevelVars {
         level, depth :: Int,	-- level of root is 0; level + depth = draft
         curmv        :: Move,	-- current move searched at this level
         alpha, beta  :: Int,	-- current alpha & beta
         moves, path  :: [Move]	-- remaining moves and best path
     }

data ResVars = ResVars {
         rscor :: Int,		-- the score of the last node return
         rpath :: [Move]	-- path of the last node return
     }

-- Actually this is search and qsearch in one
search :: Int -> Int -> Int -> Search m (Int, [Move])
search alpha beta draft = do
    es <- genEdges	-- generate the root moves
    let tv = TreeVars  { draft = draft, stans = 1 }
        lv = LevelVars { level = 0, depth = draft, alpha = alpha, beta = beta, moves = es }
    traverse tv lv ResVars firstMove nextMove getRes

getRes = id	-- ((score, path, rmvs), finst)

-- Called in preparing the first move of the current level
-- (i.e. the first node of the next level, aka one level deeper)
firstMove _ = do
    lv <- getLevelState
    if null (moves lv)
       then finalNode	-- we have no valid moves
       else nextLevel >>= noRes	-- go deeper

-- Second and further moves at the same level
nextMove r = do
    lift undoMove	-- undo the previous move
    lv <- getLevelState
    -- update node count after undo move
    if depth lv <= 0
       then modifyTreeState $ \t -> t { stans = stans t + 1, staqs = staqs t + 1 }	
       else modifyTreeState $ \t -> t { stans = stans t + 1 }
    if rscor r >= beta lv
       then betaCut r
       else do
          when (rscor r > alpha lv) $
              modifyLevelState $ \l -> l { alpha = rscor r, path = curmv l : rpath r }
          case moves lv of
              [] -> bestPath 
              _  -> nextLevel

-- When a beta cut occurs, we abort the current level
-- and return beta (negated) and the refutation path
betaCut r = do
  lv <- getLevelState
  return $ Left r { rscor = - beta lv, rpath = curmv lv : rpath r }

-- When returning from a deeper level, we need to setup the result
-- to contain the (negated) score and path of that level
bestPath = do
    lv <- getLevelState
    return $ ResVars { rscor = - alpha lv, rpath = path lv }

-- Make move to go to the next level
-- Here we always have a next move (moves lv /= [])
nextLevel = do
    lv <- getLevelState
    let (e : es) = moves lv
    putLevelState lv { curmv = e, moves = es }
    lift $ doMove e
    nes <- if depth lv > 1
              then lift genMoves	-- next level is at least depth >= 1
              else lift genTactMoves	-- next level is depth <= 0	-- start of qearch
    let lv' = LevelVars { level = level lv + 1, depth = depth lv - 1, curmv = e,	-- e!!!
                          alpha = - beta lv, beta = - alpha lv, moves = nes, path = [] }
    return $ Right lv'

-- As long as the 2 functions (firstMove and nextMove) need to return
-- different types, we need this helper
noRes x = case x of
              Left x'  -> Left x'
              Right x' -> Right (x', undefined)

finalNode = do
    v <- lift staticVal	-- the static evaluation
    return $ Left ResVars { rscor = v, rpath = [] }
