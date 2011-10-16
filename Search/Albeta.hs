{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Search.Albeta (
    alphaBeta, logmes
) where

import Control.Monad
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Data.Array.Base
import Data.Array.Unboxed

import Search.SearchMonad
import Search.AlbetaTypes
import Struct.Struct
import Struct.Status hiding (stats)
import Moves.BaseTypes
import Moves.Base

debug = False

-- Parameter for aspiration
useAspirWin = True
aspIncr :: UArray Int Int
aspIncr = array (1, 3) [ (1, 128), (2, 32), (3, 8) ]
aspTries = 3
-- Aspiration parameter optimization - 300 games:
-- First digit: tries, second: version (see below)
-- a21 = 64, 8		-> elo  -8 +- 59
-- a22 = 64, 16		-> elo  -2 +- 58
-- a23 = 128, 16	-> elo -32 +- 60
-- a31 = 64, 16, 4	-> elo -10 +- 57
-- a32 = 128, 32, 8	-> elo +53 +- 60 --> this is it
-- a33 = 100, 20, 4	-> elo   0 +- 58

-- Some fix search parameter
depthForCM  = 7 -- from this depth inform current move
minToStore  = 2 -- minimum remaining depth to store the position in hash
minToRetr   = 2 -- minimum remaining depth to retrieve
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
futilMargins = array (1, 3) [ (1, 325), (2, 550), (3, 900) ]
-- futilMargins = array (1, 3) [ (1, 500), (2, 750), (3, 1200) ]
-- futilMargins = array (1, 3) [ (1, 600), (2, 900), (3, 1200) ]

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
minIIDApply = 7
maxIIDDepth = 4

-- Parameter for quiescenst search
inEndlessCheck, qsDelta :: Int
inEndlessCheck = -1	-- there is a risk to be left in check
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
          albest :: !Bool	-- always choose the best move (i.e. first)
    } deriving Show

data PVState
    = PVState {
          ronly :: PVReadOnly,	-- read only parameters
          draft :: !Int,	-- root search depth
          absdp :: !Int,	-- absolute depth (root = 0)
          usedext :: !Int,	-- used extension
          stats :: SStats	-- search statistics
      } deriving Show

-- This is a state which reflects the status of alpha beta in a node while going through the edges
data NodeState
    = NSt {
          ownnt :: !NodeType,	-- expected node type
          forpv :: !Bool,	-- still searching for PV?
          cursc :: Path,	-- current alpha value (now plus path & depth)
          movno :: !Int,	-- current move number
          pvsl  :: [Pvsl],	-- principal variation list (at root) with node statistics
          weak  :: !Bool,	-- to recognize all nodes
          killer :: Killer, -- the current killer moves
          pvcont :: Seq Move	-- a pv continuation from the previous iteration, if available
      }

data SStats = SStats {
        sNodes, sNodesQS :: !Int,
        sCuts, sCutMovNo :: !Int,
        sRese, sNulWind  :: !Int,
        sRetr, sRSuc     :: !Int
    } deriving Show

data NodeType = PVNode | CutNode | AllNode deriving (Eq, Show)

deepNodeType PVNode  = PVNode
deepNodeType CutNode = AllNode
deepNodeType AllNode = CutNode

nextNodeType PVNode = CutNode
nextNodeType t      = t

newtype Alt e = Alt { unalt :: [e] } deriving Show
newtype Seq e = Seq { unseq :: [e] } deriving Show

firstMove = head . unseq

data Path
    = Path {
         pathScore :: !Int,
         pathDepth :: !Int,
         pathMoves :: Seq Move,
         pathOrig  :: String
      } deriving Show

-- Making a path from a plain score:
pathFromScore :: String -> Int -> Path
pathFromScore ori s = Path { pathScore = s, pathDepth = 0, pathMoves = Seq [], pathOrig = ori }

-- Add a move to a path:
addToPath :: Move -> Path -> Path
addToPath e p = p { pathDepth = pathDepth p + 1, pathMoves = Seq $ e : unseq (pathMoves p) }

-- Take only the score from a path (to another), rest empty
onlyScore :: Path -> Path
onlyScore p = Path { pathScore = pathScore p, pathDepth = 0, pathMoves = Seq [],
                  pathOrig = "onlyScore from " ++ pathOrig p }

-- Take all from the first path, except the score, which comes from the second (for fail hard)
combinePath :: Path -> Path -> Path
combinePath p1 p2 = p1 { pathScore = pathScore p2,
                         pathOrig = "(" ++ pathOrig p1 ++ ") <+> (" ++ pathOrig p2 ++ ")" }

instance Eq Path where
    p1 == p2 = pathScore p1 == pathScore p2 && pathDepth p1 == pathDepth p2

instance Ord Path where
    compare p1 p2 = ord
        where !ord = if pathScore p1 < pathScore p2
                       then LT
                       else if pathScore p1 > pathScore p2
                            then GT
                            else EQ

instance Num Path where
    p1 + p2 = Path { pathScore = pathScore p1 + pathScore p2, pathDepth = d, pathMoves = l, pathOrig = po }
        where (d, l) = if pathDepth p2 > pathDepth p1
                          then (pathDepth p2, pathMoves p2)
                          else (pathDepth p1, pathMoves p1)
              po = pathOrig p1 ++ " + " ++ pathOrig p2
    p1 * p2 = error "Path multiplication!"
    negate p = p { pathScore = negate (pathScore p) }
    abs p = p { pathScore = abs (pathScore p) }
    signum p = p { pathScore = signum (pathScore p) }
    fromInteger i = pathFromScore "fromInteger" (fromInteger i)

instance Bounded Path where
    minBound = Path { pathScore = minBound, pathDepth = 0, pathMoves = Seq [], pathOrig = "" }
    maxBound = Path { pathScore = maxBound, pathDepth = 0, pathMoves = Seq [], pathOrig = "" }

noMove :: Alt Move -> Bool
noMove (Alt es) = null es

nullSeq :: Seq Move -> Bool
nullSeq (Seq es) = null es

emptySeq :: Seq Move
emptySeq = Seq []

pvsInit = PVState { ronly = pvro00, draft = 0, absdp = 0, usedext = 0, stats = stt0 }
nst0 :: NodeState
nst0 = NSt { ownnt = PVNode, forpv = True, cursc = 0, movno = 1, weak = True,
             killer = NoKiller, pvsl = [], pvcont = emptySeq }
stt0 = SStats { sNodes = 0, sNodesQS = 0, sCuts = 0, sCutMovNo = 0, sRese = 0,
                sNulWind = 0, sRetr = 0, sRSuc = 0 }
pvro00 = PVReadOnly { school = False, albest = False }

alphaBeta :: Node m => ABControl -> m (Int, [Move], [Move])
alphaBeta abc = do
    let !d = maxdepth abc
        rmvs = Alt $ rootmvs abc
        lpv  = Seq $ lastpv abc
        searchReduced a b = pvRootSearch a      b     d lpv rmvs True
        searchLow       b = pvRootSearch alpha0 b     d lpv rmvs True
        searchHigh    a   = pvRootSearch a      beta0 d lpv rmvs True
        searchFull        = pvRootSearch alpha0 beta0 d lpv rmvs False	-- ???
        pvro = PVReadOnly { school = learnev abc, albest = best abc }
        -- pvs0 = if learnev abc then pvsInit { ronly = pvro1 } else pvsInit
        pvs0 = pvsInit { ronly = pvro } :: PVState
    r <- if useAspirWin
         then case lastscore abc of
             Just sp -> do
                let !alpha1 = sp - window abc
                    !beta1  = sp + window abc
                informStr $ "+++ Aspi search with d = " ++ show d
                               ++ " alpha = " ++ show alpha1
                               ++ " beta = " ++ show beta1
                -- aspirWin alpha1 beta1 d lpv rmvs aspTries
                r1@(s1, es1, _) <- liftM fst $ runSearch (searchReduced alpha1 beta1) pvs0
                if s1 > alpha1 && s1 < beta1 && not (nullSeq es1)
                -- if s1 > alpha1 && not (nullSeq es1)
                    then return r1
                    else do
                        informStr $ "+++ Redo search with d = " ++ show d
                               ++ " after s1 = " ++ show s1
                        liftM fst $ runSearch searchFull pvs0
             Nothing -> do
                informStr $ "+++ Full search with d = " ++ show d
                liftM fst $ runSearch searchFull pvs0
         else liftM fst $ runSearch searchFull pvs0
    return $! case r of (s, Seq path, Alt rmvs) -> (s, path, rmvs)

aspirWin :: Node m => Int -> Int -> Int -> Seq Move -> Alt Move -> Int -> m (Int, Seq Move, Alt Move)
aspirWin _ _ d lpv rmvs 0 = liftM fst $ runSearch (pvRootSearch alpha0 beta0 d lpv rmvs True) pvsInit
aspirWin a b d lpv rmvs t = do
    r@(s, p, ms) <- liftM fst $ runSearch (pvRootSearch a b d lpv rmvs True) pvsInit
    if s <= a
       then aspirWin (a - incr) b d lpv rmvs (t-1)
       else if s >= b
            then aspirWin a (b + incr) d p ms (t-1)
            else if nullSeq p
                    then aspirWin (a - incr) (b + incr) d lpv rmvs (t-1)
                    else return r
    where incr = aspIncr!t

-- Root PV Search
pvRootSearch :: Node m => Int -> Int -> Int -> Seq Move -> Alt Move -> Bool
             -> Search m (Int, Seq Move, Alt Move)
pvRootSearch a b d _ _ _ | d <= 0 = do	-- this part is only for eval learning
    v <- pvQSearch a b 0		-- in normal play always d >= 1
    return (v, emptySeq, Alt [])
pvRootSearch a b d lastpath rmvs aspir = do
    modify $ \s -> s { draft = d }
    -- Root is pv node, cannot fail low, except when aspiration fails!
    edges <- if null (unalt rmvs)
                then genAndSort lastpath NoKiller d True
                else if null (unseq lastpath)
                        then return rmvs
                        else do
                           let !lm = firstMove lastpath
                           return $ Alt $ lm : delete lm (unalt rmvs)
    -- lift $ informStr $ "Root moves: " ++ show edges
    -- pvcont is the pv continuation from the last iteration
    let !pvc  = if nullSeq lastpath then lastpath else Seq $ tail $ unseq lastpath
        !nsti = nst0 { cursc = pathFromScore "Alpha" a, pvcont = pvc }
    nstf <- pvLoop (pvInnerRoot (pathFromScore "Beta" b) d) nsti edges
    rsr <- if weak nstf		-- failed low
              then do
                when (not aspir) $ do
                     s <- get
                     lift $ logmes $ "Failed low at root! Status: " ++ show s
                return (a, emptySeq, edges)	-- just to permit aspiration to retry
              else do
                 albest' <- gets (albest . ronly)
                 (s, p) <- if (pathScore (cursc nstf) >= b)
                              then return (pathScore (cursc nstf), unseq $ pathMoves (cursc nstf))
                              else lift $ choose albest'
                                        $ sortBy (comparing fstdesc)
                                        $ map pvslToPair
                                        $ filter pvGood $ pvsl nstf
                 when (d < depthForCM) $ informBest s d p
                 let !best = head p
                     !xrmvs = Alt $ best : delete best (unalt edges)	-- best on top
                 return (s, Seq p, xrmvs)
    reportStats
    lift $ informStr $ "*** Killers at root: " ++ show (killer nstf)
    return rsr
    where fstdesc (a, _) = -a

pvslToPair :: Pvsl -> (Int, [Move])
pvslToPair (Pvsl { pvPath = p }) = (score, pv)
    where pv = unseq $ pathMoves p
          de = pathDepth p
          sc = pathScore p
          score = scoreToExtern sc de

-- The internal score is for weird for found mates (always mate)
-- Turn it to nicer score by considering path lenght to mate
scoreToExtern :: Int -> Int -> Int
scoreToExtern sc de
    | nearmate sc = if sc > 0 then sc - de else sc + de
    | otherwise   = sc

-- This is the inner loop of the PV search of the root, executed at root once per possible move
-- See the parameter
-- Returns: ...flag if it was a beta cut and new status
pvInnerRoot :: Node m
            => Path 	-- current beta
            -> Int	-- current search depth
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search m (Bool, NodeState)
pvInnerRoot b d nst e = do
    -- when debug $ logmes $ "--> pvInner: b d old: " ++ show b ++ ", " ++ show d ++ ", " ++ show old
    -- checkMe b "pvInnerRoot 1"
    old <- get
    let !mn = movno nst
    when (draft old >= depthForCM) $ lift $ informCM e mn
    -- pindent $ "-> " ++ show e
    -- lift $ logmes $ "Search root move " ++ show e ++ " a = " ++ show a ++ " b = " ++ show b
    -- do the move
    exd <- lift $ doEdge e False
    newNode
    modify $ \s -> s { absdp = absdp s + 1 }
    s <- case exd of
             Exten exd' -> pvInnerRootExten b d (special e) exd' nst
             Final sco  -> return $! pathFromScore "Final" sco
    -- checkMe s "pvInnerRoot 2"
    -- undo the move
    lift $ undoEdge e
    modify $ \s -> s { absdp = absdp old, usedext = usedext old }
    let s' = addToPath e (pnextlev s)
    -- -- checkMe s' "pvInnerRoot 3"
    -- pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
    -- lift $ informStr $ "<- " ++ show e ++ " (" ++ show (pathScore s)
    --                                 ++ " /// " ++ show (pathScore s') ++ ")"
    checkFailOrPVRoot (stats old) b d e s' nst

pvInnerRootExten :: Node m => Path -> Int -> Bool -> Int -> NodeState -> Search m Path
pvInnerRootExten b d spec exd nst = do
    -- pindent $ "depth = " ++ show d
    -- checkMe b "pvInnerRootExten 1"
    old <- get
    exd' <- reserveExtension (usedext old) exd
    tact <- lift tactical
    let reduce = okToReduce tact
        (!d', reduced) = nextDepth (d+exd') (movno nst) reduce (forpv nst && a < b - 1)
    -- pindent $ "exd' = " ++ show exd'
    pvpath' <- if nullSeq $ pvcont nst then bestMoveFromHash else return (pvcont nst)
    pvpath  <- if useIID && nullSeq pvpath'
                  then bestMoveFromIID nst (-a-1) (-a) d' nulMoves
                  else return pvpath'
    -- when (d' < d-1) $ pindent $ "d' = " ++ show d'
    -- checkMe a $ "pvInnerRootExten 2:" ++ show a
    if forpv nst
       then pvSearch nst (-b) (-a) d' pvpath nulMoves
       else do
           -- no futility pruning for root moves!
           nulWind
           -- lift $ informStr $ "Search with closed window a = " ++ show (-a-1)
           --            ++ " b = " ++ show (-a) ++ " depth " ++ show d'
           s1 <- pvSearch nst (-a-1) (-a) d' pvpath nulMoves
           -- -- checkMe s1 "pvInnerRootExten 3"
           if -s1 > a -- we didn't fail low, so we need re-search
              then do
                 reSearch	-- here: node type?
                 -- pindent $ "Research! (" ++ show s1 ++ ")"
                 pvpath'' <- if useIID then bestMoveFromIID nst (-b) (-a) d' nulMoves else return pvpath
                 let nst' = nst { ownnt = PVNode }
                 if reduced && d > 1
                    then do	-- re-search with no reduce for root moves
                      -- let d''= fst $! nextDepth (d+exd') (draft old) (movno nst) False (forpv nst)
                      let d''= fst $! nextDepth (d+exd') (movno nst) False True
                      pvSearch nst' (-b) (-a) d'' pvpath'' nulMoves
                    else pvSearch nst' (-b) (-a) d' pvpath'' nulMoves
              else return s1
    where !inPv = ownnt nst == PVNode
          a = cursc nst
          okToReduce t = lmrActive && not (inPv || spec || exd > 0 || d < lmrMinDRed) && not t

checkFailOrPVRoot :: Node m => SStats -> Path -> Int -> Move -> Path
                  -> NodeState -> Search m (Bool, NodeState)
checkFailOrPVRoot xstats b d e s nst = do
    -- checkMe b "checkFailOrRoot 1"
    -- checkMe s "checkFailOrRoot 2"
    sst <- get
    let !mn     = movno nst
        !a      = cursc nst
        -- !np     = pathMoves s
        !nodes0 = sNodes xstats + sRetr xstats
        !nodes1 = sNodes (stats sst) + sRetr (stats sst)
        !nodes  = nodes1 - nodes0
        pvg    = Pvsl s nodes True	-- the good
        pvb    = Pvsl s nodes False	-- the bad
        -- xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
        -- xpvslb = insertToPvs d pvb (pvsl nst)	-- the bad
        de = pathDepth s
    -- logmes $ "*** to pvsl: " ++ show xpvsl
    inschool <- gets $ school . ronly
    if d == 1	-- for depth 1 search we search all exact
       then do
            let typ = 2
            when (de >= minToStore) $ lift $ store de typ (pathScore s) e nodes
            -- when inschool $ do
            --     s0 <- pvQSearch a b 0
            --     lift $ learn d typ s s0
            let nst1 = if s > a
                          then nst { cursc = s, ownnt = nextNodeType (ownnt nst),
                                     forpv = False, weak = False }
                          else nst
            xpvslg <- insertToPvs d pvg (pvsl nst)	-- the good
            return (False, nst1 {movno = mn + 1, pvsl = xpvslg, pvcont = emptySeq})
       else if s >= b
               then do	-- what when a root move fails high? We are in aspiration
                    let typ = 1	-- best move is e and is beta cut (score is lower limit)
                    when (de >= minToStore) $ lift $ store de typ (pathScore s) e nodes
                    -- when inschool $ do
                    --     s0 <- pvQSearch a b 0
                    --     lift $ learn d typ b s0
                    lift $ betaMove True d (absdp sst) e
                    put sst { stats = statCut (stats sst) mn }
                    xpvslg <- insertToPvs d pvg (pvsl nst)	-- the good
                    let nst1 = nst { cursc = csc, weak = False, pvsl = xpvslg, pvcont = emptySeq }
                        !csc = if s > b then combinePath s b else s
                    -- lift $ logmes $ "Root move " ++ show e ++ " failed high: " ++ show s
                    -- lift $ informStr $ "Cut (" ++ show b ++ "): " ++ show np
                    return (True, nst1)
               else if s > a
                    then do
                         -- lift $ informStr $ "Next info: " ++ pathOrig s
                         let sc = pathScore s
                             pa = unseq $ pathMoves s
                             le = length pa
                         informBest (scoreToExtern sc le) (draft sst) pa
                         let typ = 2	-- best move so far (score is exact)
                         when (de >= minToStore) $ lift $ store de typ (pathScore s) e nodes
                         -- when inschool $ do
                         --     s0 <- pvQSearch a b 0
                         --     lift $ learn d typ s s0
                         xpvslg <- insertToPvs d pvg (pvsl nst)	-- the good
                         let nst1 = nst { cursc = s, ownnt = nextNodeType (ownnt nst),
                                          forpv = False, movno = mn + 1,
                                          weak = False, pvsl = xpvslg, pvcont = emptySeq }
                         -- lift $ logmes $ "Root move " ++ show e ++ " improves alpha: " ++ show s
                         -- lift $ informStr $ "Better (" ++ show s ++ "):" ++ show np
                         return (False, nst1)
                    else do
                         -- when in a cut node and the move dissapointed - negative history
                         when (useNegHist && forpv nst && a == b - 1 && mn <= negHistMNo)
                              $ lift $ betaMove False d (absdp sst) e
                         if (forpv nst)
                            then return (True, nst { cursc = s })	-- i.e we failed low in aspiration
                            else do
                              let es = unseq $ pathMoves s
                                  mm = head es
                                  km = head $ drop 1 es
                                  s1 = - pathScore s
                              kill1 <- if d >= 2 && moreThanOne es
                                          then do
                                              iskm <- lift $ killCandEdge mm km
                                              if iskm then return $! pushKiller km s1 (killer nst)
                                                      else return $ killer nst
                                          else return $ killer nst
                              xpvslb <- insertToPvs d pvb (pvsl nst)	-- the bad
                              let nst1 = nst { movno = mn + 1, pvsl = xpvslb,
                                               killer = kill1, pvcont = emptySeq }
                              return (False, nst1)

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

-- PV Search
pvSearch :: Node m => NodeState -> Path -> Path -> Int -> Seq Move -> Int
                       -> Search m Path
pvSearch _ !a !b !d _ _ | d <= 0 = do
    -- checkMe a "pvSearch 1"
    -- checkMe b "pvSearch 2"
    v <- pvQSearch (pathScore a) (pathScore b) 0
    when debug $ lift $ logmes $ "<-- pvSearch: reach depth 0, return " ++ show v
    -- let !v' = if v <= a then a else if v > b then b else v
    -- pindent $ "<> " ++ show v
    return $! pathFromScore ("pvQSearch 1:" ++ show v) v
pvSearch nst !a !b !d lastpath lastnull = do
    -- pindent $ "=> " ++ show a ++ ", " ++ show b
    -- checkMe a "pvSearch 3"
    -- checkMe b "pvSearch 4"
    nmfail <- nullEdgeFailsHigh nst b d lastnull
    if nmfail
       -- then pindent ("<= " ++ show b) >> return (onlyScore b)
       then return $! onlyScore b
       else do
          edges <- genAndSort lastpath (killer nst) d (forpv nst)	-- here: (ownnt nst /= AllNode)
          if noMove edges
             then do
                  v <- lift staticVal
                  -- pindent ("<= " ++ show v)
                  return $! pathFromScore ("static: " ++ show v) v
             else do
                  -- nodes0 <- gets stats >>= return . sNodes
                  nodes0 <- gets (sNodes . stats)
                  -- Loop thru the moves
                  let !pvpath = if nullSeq lastpath then emptySeq else Seq $ tail $ unseq lastpath
                      !nsti = nst0 { ownnt = deepNodeType (ownnt nst), cursc = a, pvcont = pvpath }
                  nstf <- pvLoop (pvInnerLoop b d) nsti edges
                  nodes1 <- gets (sNodes . stats)
                  s <- if weak nstf
                          then do
                              inschool <- gets $ school . ronly
                              -- when inschool $ do
                              --     s0 <- pvQSearch a b 0
                              --     lift $ learn d typ s s0
                              let s = cursc nstf
                                  de = pathDepth s
                              when (de >= minToStore) $ do
                                  let typ = 0
                                      !deltan = nodes1 - nodes0
                                  -- store as upper score - move does not matter - tricky here!
                                  lift $ store de typ (pathScore s) (head $ unalt edges) deltan
                              return $! onlyScore s
                          else return $! cursc nstf	-- modify $ \st -> st { killer = kill1 }
                  -- checkMe s "pvSearch 5"
                  -- pindent $ "<= " ++ show s
                  return s

nullEdgeFailsHigh :: Node m => NodeState -> Path -> Int -> Int -> Search m Bool
nullEdgeFailsHigh nst b d lastnull =
    if not nulActivate || lastnull < 1
       then return False
       else do
         tact <- lift tactical
         if tact
            then return False
            else do
               -- checkMe b "nullEdgeFailsHigh 1"
               lift nullEdge	-- do null move
               inschool <- gets $ school . ronly
               let !nmb = if nulSubAct && not inschool then b - snulSubmrg else b
                   !d1  = d - 1 - nulRedux
                   !lastnull1 = lastnull - 1
               val <- pvSearch nst (-nmb) (-nmb + snulMargin) d1 (emptySeq) lastnull1
               lift nullEdge	-- undo null move
               return $! (-val) >= nmb
    where snulMargin = pathFromScore "nulMargin" nulMargin
          snulSubmrg = pathFromScore "nulSubmrg" nulSubmrg

-- This is the inner loop of the PV search, executed at every level (except root) once per possible move
-- See the parameter
-- Returns: flag if it was a beta cut and new status
pvInnerLoop :: Node m
            => Path 	-- current beta
            -> Int	-- current search depth
            -> NodeState 	-- node status
            -> Move	-- move to search
            -> Search m (Bool, NodeState)
pvInnerLoop b d nst e = do
    -- checkMe b "pvInnerLoop 1"
    old <- get
    -- pindent $ "-> " ++ show e
    exd <- lift $ doEdge e False	-- do the move
    newNode
    modify $ \s -> s { absdp = absdp s + 1 }
    s <- case exd of
             Exten exd' -> pvInnerLoopExten b d (special e) exd' nst
             Final sco  -> return $! pathFromScore "Final" sco
    -- checkMe s "pvInnerLoop 2"
    lift $ undoEdge e	-- undo the move
    modify $ \s -> s { absdp = absdp old, usedext = usedext old }
    let s' = addToPath e (pnextlev s)
    -- -- checkMe s' "pvInnerLoop 3"
    -- pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
    checkFailOrPVLoop (stats old) b d e s' nst

reserveExtension !uex !exd = do
    if uex >= maxDepthExt || exd == 0
       then return 0
       else do
            modify $ \s -> s { usedext = usedext s + exd }
            return exd

pvInnerLoopExten :: Node m => Path -> Int -> Bool -> Int -> NodeState
                 -> Search m Path
pvInnerLoopExten b d spec exd nst = do
    -- checkMe b "pvInnerLoopExten 1"
    old <- get
    tact <- lift tactical
    exd' <- reserveExtension (usedext old) exd
    let mn = movno nst
        -- late move reduction
        !reduce = okToReduce tact
        !de = d + exd'
        (!d', reduced) = nextDepth de mn reduce (forpv nst && a < b - 1)
    -- checkMe a "pvInnerLoopExten 2"
    if forpv nst
       then do
          pvpath <- if useIID && nullSeq (pvcont nst) then bestMoveFromHash else return (pvcont nst)
          pvSearch nst (-b) (-a) d' pvpath nulMoves
       else do
          (hdeep, tp, hscore, e', nodes)
              <- if d >= minToRetr
                    then reTrieve >> lift retrieve
                    else return (-1, 0, 0, undefined, 0)
          -- let pvpath = if null lastpath
          --           then if hdeep > 0 && tp > 0 then [e'] else []
          --           else lastpath
          let pvpath' = if hdeep > 0 && tp > 0 then Seq [e'] else (pvcont nst)
          --1-- let !pvpath = if hdeep > 0 && tp > 0 then Seq [] else (pvcont nst)
              -- !hs = nextlev hscore
              ttpath = Path { pathScore = hscore, pathDepth = hdeep, pathMoves = Seq [e'],
                              pathOrig = "TT" }
              --2-- ttpath = Path { pathScore = hscore, pathDepth = hdeep, pathMoves = Seq [] }
              hs = - ttpath
          if hdeep >= d && (tp == 2 || tp == 1 && hs > a || tp == 0 && hs <= a)
             then reSucc nodes >> return ttpath
             else do
                 -- futility pruning
                 inschool <- gets $ school . ronly
                 (!prune, !v) <- if not futilActive || tact || inschool
                                  -- don't prune when tactical or in learning
                                  then return (False, 0)
                                  else isPruneFutil d (-b) (-a)
                 -- -- checkMe v "pvInnerLoopExten 3"
                 if prune
                    then return v	-- we will fail low or high
                    else do
                       nulWind
                       pvpath <- if useIID && nullSeq pvpath'
                                    then bestMoveFromIID nst (-a-1) (-a) d' nulMoves
                                    else return pvpath'
                       !s1 <- pvSearch nst (-a-1) (-a) d' pvpath nulMoves
                       -- -- checkMe s1 "pvInnerLoopExten 4"
                       if -s1 > a -- we need re-search
                          then do
                            reSearch
                            -- here: next node type?
                            pvpath'' <- if useIID then bestMoveFromIID nst (-b) (-a) d' nulMoves else return pvpath
                            let nst' = nst { ownnt = PVNode }
                            if reduced && d >= lmrMinDFRes
                               then do	-- re-search with no reduce is expensive!
                                  let !d'' = fst $ nextDepth (d+exd') mn False (ownnt nst == PVNode)
                                  pvSearch nst' (-b) (-a) d'' pvpath'' nulMoves
                               else pvSearch nst' (-b) (-a) d' pvpath'' nulMoves
                          else return s1
    where !inPv = ownnt nst == PVNode
          a = cursc nst
          okToReduce t = lmrActive
                           && not (inPv || pnearmate a || spec || exd > 0 || d < lmrMinDRed)
                           && not t

pnearmate = nearmate . pathScore
pnextlev p = p { pathScore = nextlev (pathScore p) }

checkFailOrPVLoop :: Node m => SStats -> Path -> Int -> Move -> Path
                  -> NodeState -> Search m (Bool, NodeState)
checkFailOrPVLoop xstats b d e s nst = do
    -- checkMe b "checkFailOrPVLoop 1"
    -- checkMe s "checkFailOrPVLoop 2"
    sst <- get
    let !mn = movno nst
        !a  = cursc nst
        !nodes0 = sNodes xstats
        !nodes1 = sNodes $ stats sst
        !nodes  = nodes1 - nodes0
        de = pathDepth s
    inschool <- gets $ school . ronly
    -- checkMe a "checkFailOrPVLoop 3"
    if s >= b
       then do
            let typ = 1	-- best move is e and is beta cut (score is lower limit)
            when (de >= minToStore) $ lift $ store de typ (pathScore s) e nodes
            lift $ betaMove True d (absdp sst) e -- anounce a beta move (for example, update history)
            -- when inschool $ do
            --     s0 <- pvQSearch a b 0
            --     lift $ learn d typ b s0
            -- when debug $ logmes $ "<-- pvInner: beta cut: " ++ show s ++ ", return " ++ show b
            put sst { stats = statCut (stats sst) mn }
            let nst1 = nst { cursc = csc, weak = False, pvcont = emptySeq }
                !csc = if s > b then combinePath s b else s
            -- lift $ informStr $ "Cut (" ++ show b ++ "): " ++ show np
            return (True, nst1)
       else if s > a
          then do
              let typ = 2	-- score is exact
              when (ownnt nst == PVNode || de >= minToStore) $ lift $ store de typ (pathScore s) e nodes
              -- when debug $ logmes $ "<-- pvInner - new a: " ++ show s
              -- when inschool $ do
              --     s0 <- pvQSearch a b 0
              --     lift $ learn d typ s s0
              let !mn1 = mn + 1
                  nst1 = nst { cursc = s, ownnt = nextNodeType (ownnt nst),
                               forpv = False, movno = mn1, weak = False, pvcont = emptySeq }
              -- lift $ informStr $ "Better (" ++ show s ++ "): " ++ show np
              return (False, nst1)
          else do
              -- when in a cut node and the move dissapointed - negative history
              when (useNegHist && forpv nst && a == b - 1 && mn <= negHistMNo)
                   $ lift $ betaMove False d (absdp sst) e
              let !mn1 = mn + 1
                  es = unseq $ pathMoves s
                  mm = head es
                  km = head $ drop 1 es
                  s1 = - pathScore s
              kill1 <- if d >= 2 && moreThanOne es
                          then do
                              iskm <- lift $ killCandEdge mm km
                              if iskm then return $! pushKiller km s1 (killer nst)
                                      else return $ killer nst
                          else return $ killer nst
              let nst1 = nst { movno = mn1, killer = kill1, pvcont = emptySeq }
              return (False, nst1)

-- We don't sort the moves here, they have to come sorted from genEdges
-- But we consider the best moves first (best from previous iteration, killers)
genAndSort :: Node m => Seq Move -> Killer -> Int -> Bool -> Search m (Alt Move)
genAndSort lastpath kill d pv = do
    adp <- gets absdp 
    kl <- lift $ filterM legalEdge $ killerToList kill
    esp <- lift $ genEdges d adp pv'
    let es = bestFirst (unseq lastpath) kl esp
    return $ Alt es
    where pv' = pv || not (nullSeq lastpath)

-- Late Move Reduction
-- {-# INLINE nextDepth #-}
nextDepth :: Int -> Int -> Bool -> Bool -> (Int, Bool)
nextDepth !d !w !lmr !pv = (m0d, reduced)
    where !nd = if lmr then d - k else d1
          !idx = (min lmrMaxDepth d, min lmrMaxWidth w)
          k  = if pv then lmrReducePv ! idx else lmrReduceArr ! idx
          -- dg = (d + deGran - 1) `div` deGran
          !reduced = nd < d1
          !d1 = d - 1
          !m0d = max 0 nd

-- This is a kind of monadic fold optimized for (beta) cut
-- {-# INLINE pvLoop #-}
pvLoop :: Monad m => (s -> e -> m (Bool, s)) -> s -> Alt e -> m s
pvLoop _ s (Alt [])     = return s
pvLoop f s (Alt (e:es)) = do
    (cut, s') <- f s e
    if cut then return s'
           else pvLoop f s' $ Alt es

isPruneFutil :: Node m => Int -> Path -> Path -> Search m (Bool, Path)
isPruneFutil d a b
    | d > maxFutilDepth = return (False, 0)
    | otherwise = do
        -- checkMe a "isPruneFutere 1"
        -- checkMe b "isPruneFutere 2"
        let !margin = futilMargins ! d
        -- v <- lift materVal	-- can we do here direct static evaluation?
        v <- pvQSearch (pathScore a) (pathScore b) 0
        let a' = pathScore a
            b' = pathScore b
        if v < a' && v + margin <= a'
           then return (True, onlyScore a)
           else if v > b' && v - margin >= b'
                then return (True, onlyScore b)
                else return (False, 0)

-- PV Quiescent Search
pvQSearch :: Node m => Int -> Int -> Int -> Search m Int
pvQSearch a b c = do				   -- to avoid endless loops
    -- qindent $ "=> " ++ show a ++ ", " ++ show b
    stp <- lift staticVal				-- until we can recognize repetition
    !tact <- lift tactical
    if tact
       then do
           (es1, es2) <- lift $ genEdges 0 0 False
           let edges = Alt $ es1 ++ es2
           if noMove edges
              -- then qindent ("<= " ++ show stp) >> return stp
              then return stp
              else if c >= qsMaxChess
                      -- then qindent ("<= -1") >> return inEndlessCheck
                      then return inEndlessCheck
                      else do
                          -- for check extensions in case of very few moves (1 or 2):
                          -- if 1 move: search even deeper
                          -- if 2 moves: same depth
                          -- if 3 or more: no extension
                          -- let !esc = length $ take 3 $ unalt edges
                          let !esc = lenmax3 $ unalt edges
                              !nc = c + esc - 2
                              !a' = if stp > a then stp else a
                          !s <- pvLoop (pvQInnerLoop b nc) a' edges
                          -- qindent $ "<= " ++ show s
                          return s
       else if qsBetaCut && stp >= b
               -- then qindent ("<= " ++ show b) >> return b
               then return b
               else do
                   let !delta = a - qsDelta
                   if qsDeltaCut && delta < a && stp < delta
                      -- then qindent ("<= " ++ show a) >> return a
                      then return a
                      else do
                          edges <- liftM Alt $ lift genTactEdges
                          if noMove edges
                             -- then qindent ("<= " ++ show stp) >> return stp
                             then return stp
                             else do
                                 let !a' = if stp > a then stp else a
                                 !s <- pvLoop (pvQInnerLoop b c) a' edges
                                 -- qindent $ "<= " ++ show s
                                 return s
    where lenmax3 as = lenmax3' 0 as
          lenmax3' !n _ | n == 3 = 3
          lenmax3' !n []         = n
          lenmax3' !n (a:as)     = lenmax3' (n+1) as

pvQInnerLoop :: Node m => Int -> Int -> Int -> Move -> Search m (Bool, Int)
pvQInnerLoop b c a e = do
    -- here: delta pruning: captured piece + 200 > a? then go on, else return
    -- qindent $ "-> " ++ show e
    r <- lift $ doEdge e True
    newNodeQS
    modify $ \s -> s { absdp = absdp s + 1 }
    s <- liftM nextlev $ case r of
            Final s -> return s
            _       -> pvQSearch (-b) (-a) c
    lift $ undoEdge e
    modify $ \s -> s { absdp = absdp s - 1 }	-- don't care about usedext here
    -- qindent $ "<- " ++ show e ++ " (" ++ show s ++ ")"
    if s >= b
       then return (True, b)
       else if s > a
               then return (False, s)
               else return (False, a)

bestMoveFromHash :: Node m => Search m (Seq Move)
bestMoveFromHash = do
    reTrieve
    (hdeep, tp, _, e, _) <- lift retrieve
    when (hdeep > 0) $ reSucc 1		-- here we save just move generation
    return $! if hdeep > 0 && tp > 0 then Seq [e] else emptySeq
    -- return $! Seq [ e | hdeep > 0 && tp > 0 ]
    --3-- return $! Seq []

{-# INLINE bestMoveFromIID #-}
bestMoveFromIID :: Node m => NodeState -> Path -> Path -> Int -> Int -> Search m (Seq Move)
bestMoveFromIID nst a b !d lastnull
    | d < minIIDApply || ownnt nst == AllNode
                         = return emptySeq
    | otherwise          = pathMoves `liftM` pvSearch nst a b d' emptySeq lastnull
    where d' = min maxIIDDepth (d `div` 2)

{-# INLINE reportStats #-}
reportStats :: Node m => Search m ()
reportStats = do
    s <- get
    let !xst = stats s
        !xcu = fromIntegral (sCuts xst) :: Double
    lift $ logmes $ "Search statistics after draft " ++ show (draft s) ++ ":"
    lift $ logmes $ "Nodes: " ++ show (sNodes xst) ++ ", in QS: " ++ show (sNodesQS xst)
             ++ ", Cuts: " ++ show (sCuts xst) ++ ", average mov no: "
             ++ show (fromIntegral (sCutMovNo xst) / xcu)
             ++ ", null window: " ++ show (sNulWind xst) ++ ", research: " ++ show (sRese xst)
             ++ ", retrieve: " ++ show (sRetr xst) ++ ", rsucc: " ++ show (sRSuc xst)

-- Functions to keep statistics
modStat :: Node m => (SStats -> SStats) -> Search m ()
modStat f = modify $ \s -> case f (stats s) of st -> s { stats = st }

incNodes   s = case sNodes s + 1 of n1 -> s { sNodes = n1 }
incNodesQS s = case sNodes s + 1 of
                 n1 -> case sNodesQS s + 1 of n2 -> s { sNodes = n1, sNodesQS = n2 }
incNulWind s = case sNulWind s + 1 of n1 -> s { sNulWind = n1 }
incReSearch s = case sRese s + 1 of n1 -> s { sRese = n1 }
incReTrieve s = case sRetr s + 1 of n1 -> s { sRetr = n1 }
addReSucc n s = case sRSuc s + n of n1 -> s { sRSuc = n1 }

newNode :: Node m => Search m ()
newNode   = modStat incNodes

newNodeQS :: Node m => Search m ()
newNodeQS = modStat incNodesQS

nulWind :: Node m => Search m ()
nulWind   = modStat incNulWind

reSearch :: Node m => Search m ()
reSearch  = modStat incReSearch

reTrieve :: Node m => Search m ()
reTrieve  = modStat incReTrieve

reSucc :: Node m => Int -> Search m ()
reSucc n  = modStat (addReSucc n)

indentActive :: Node m => String -> Search m ()
indentActive s = do
    ad <- gets absdp
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

statCut :: SStats -> Int -> SStats
statCut s n = s { sCuts = sCuts s + 1, sCutMovNo = sCutMovNo s + n }

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
