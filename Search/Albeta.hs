{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Search.Albeta (
    Node(..),
    Edge(..),
    Score(..),
    DoResult(..),
    Comm(..),
    ABControl(..),
    alphaBeta, logmes
) where

import Control.Monad
import Data.List (delete, sortBy)
import Data.Ord (comparing)
import Data.Array.Base
import Data.Array.Unboxed

import Search.SearchMonad

debug = False

-- Some fix search parameter
useAspirWin = True
-- depthForCM  = 8 -- from this depth inform current move
depthForCM  = 6 -- from this depth inform current move
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
lmrPv     = 7
lmrRest   = 5
-- lmrPv     = 3.8
-- lmrRest   = 2.2
-- lmrPv     = 3.5
-- lmrRest   = 2.5
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
futilMargins :: Score s => Array Int s
-- futilMargins = array (1, 3) [ (1, 400), (2, 650), (3, 1200) ]
futilMargins = array (1, 3) [ (1, 500), (2, 750), (3, 1200) ]
-- futilMargins = array (1, 3) [ (1, 600), (2, 900), (3, 1200) ]

-- Parameters for quiescent search:
qsBetaCut  = True	-- use beta cut in QS?
qsDeltaCut = True	-- use delta prune in QS?
qsMaxChess = 2		-- max number of chess for a quiet search path

-- Parameters for null move pruning
nulActivate = True		-- activate null move reduction
nulRedux    = 3 -- depth reduction for null move
nulMoves    = 2	-- how many null moves in sequence are allowed (one or two)
nulMargin, nulSubmrg :: Score s => s
nulMargin   = 1	-- 120		-- margin to search the null move (over beta)
nulSubmrg   = 10	-- improved margin
nulSubAct   = True

-- Parameter for quiescenst search
inEndlessCheck, qsDelta :: Score s => s
inEndlessCheck = -1	-- there is a risk to be left in check
qsDelta     = 1100

class (Ord s, Num s, Bounded s) => Score s where
    nextlev :: s -> s
    nearmate :: s -> Bool

class Edge e where
    special :: e -> Bool

data ABControl e s = ABC {
        maxdepth :: !Int,
        lastpv :: [e],
        lastscore :: Maybe s,
        rootmvs :: [e],
        window :: s,
        learnev :: Bool
    } deriving Show

-- The node class, dependent on a game monad m, an edge type e (moves)
-- and a score type s
class (Monad m, Eq e, Show e, Edge e, Score s, Show s) =>
  Node m e s | m -> e, m -> s, s -> m where
    staticVal :: m s  -- static evaluation of a node
    materVal  :: m s  -- material evaluation (for prune purpose)
    genEdges :: Int -> Int -> Bool -> m [e]  -- generate all legal edges
    genTactEdges :: m [e]  -- generate all edges in tactical positions
    legalEdge :: e -> m Bool	-- is the move legal?
    inSeq :: e -> e -> m Bool	-- can 2 moves be in sequence?
    tactical :: m Bool -- if a position is tactical, search further
    doEdge   :: e -> Bool -> m (DoResult s)
    undoEdge :: e -> m ()
    betaMove :: Bool -> Int -> Int -> e -> m ()   -- called for beta-cut moves
    nullEdge :: m ()		   -- do null move (and also undo)
    retrieve :: m (Int, Int, s, e, Int)   -- retrieve the position in hash
    store :: Int -> Int -> s -> e -> Int -> m () -- store the position in hash
    learn :: Int -> Int -> s -> s -> m ()	-- learn the evaluation parameters
    curNodes :: m Int
    inform :: Comm e s -> m ()		-- communicate to the world (log, current and best move)
    choose :: [(s, [e])] -> m (s, [e])

type Search m e s = STPlus (PVState e s) m

alpha0, beta0 :: Node m e s => s
alpha0 = minBound + 2000
beta0  = maxBound - 2000

data DoResult s = Exten !Int	-- return mit extension (evtl 0)
                | Final !s	-- return with a final score (probably draw)

data Comm e s = LogMes String
              | BestMv s Int Int [e]
              | CurrMv e Int
              | InfoStr String

data Pvsl e s = Pvsl {
        pvPath :: Path e s,	-- pv path
        pvNodes :: !Int,	-- number of nodes in the current search
        pvGood  :: !Bool	-- beta cut or alpha improvement
    } deriving Show

data (Show e, Show s) => Killer e s = NoKiller | OneKiller e s | TwoKillers e s e s deriving Show

data SStats = SStats {
        sNodes, sNodesQS :: !Int,
        sCuts, sCutMovNo :: !Int,
        sRese, sNulWind  :: !Int,
        sRetr, sRSuc     :: !Int
    } deriving Show

data NodeType = PVNode | CutNode | AllNode deriving (Eq, Show)

newtype Alt e = Alt { unalt :: [e] } deriving Show
newtype Seq e = Seq { unseq :: [e] } deriving Show

data Path e s
    = Path {
         pathScore :: !s,
         pathDepth :: !Int,
         pathMoves :: Seq e
      } deriving Show

-- Making a path from a plain score:
pathFromScore :: s -> Path e s
pathFromScore s = Path { pathScore = s, pathDepth = 0, pathMoves = Seq [] }

-- Add a move to a path:
addToPath :: e -> Path e s -> Path e s
addToPath e p = p { pathDepth = pathDepth p + 1, pathMoves = Seq $ e : unseq (pathMoves p) }

-- Take only the score from a path (to another), rest empty
onlyScore :: Path e s -> Path e s
onlyScore (Path { pathScore = s }) = Path { pathScore = s, pathDepth = 0, pathMoves = Seq [] }

-- Take all from the first path, except the score, which comes from the second (for fail hard)
combinePath :: Path e s -> Path e s -> Path e s
combinePath p1 p2 = p1 { pathScore = pathScore p2 }

instance Eq s => Eq (Path e s) where
    p1 == p2 = pathScore p1 == pathScore p2 && pathDepth p1 == pathDepth p2

instance Ord s => Ord (Path e s) where
    compare p1 p2 = ord
        where !ord = if pathScore p1 < pathScore p2
                       then LT
                       else if pathScore p1 > pathScore p2
                            then GT
                            else EQ

instance (Show e, Num s) => Num (Path e s) where
    p1 + p2 = Path { pathScore = pathScore p1 + pathScore p2, pathDepth = d, pathMoves = l }
        where (d, l) = if pathDepth p2 > pathDepth p1
                          then (pathDepth p2, pathMoves p2)
                          else (pathDepth p1, pathMoves p1)
    p1 * p2 = error "Path multiplication!"
    negate p = p { pathScore = negate (pathScore p) }
    abs p = p { pathScore = abs (pathScore p) }
    signum p = p { pathScore = signum (pathScore p) }
    fromInteger i = pathFromScore (fromInteger i)

instance Bounded s => Bounded (Path e s) where
    minBound = Path { pathScore = minBound, pathDepth = 0, pathMoves = Seq [] }
    maxBound = Path { pathScore = maxBound, pathDepth = 0, pathMoves = Seq [] }

instance (Show e, Edge e, Score s) => Score (Path e s) where
    nextlev p = p { pathScore = nextlev (pathScore p) }
    nearmate  = nearmate . pathScore

-- Read only parameters of the search, so that we can change them programatically
data PVReadOnly
    = PVReadOnly {
          school :: !Bool	-- running in learning mode
    } deriving Show

data PVState e s
    = PVState {
          ronly :: PVReadOnly,	-- read only parameters
          draft :: !Int,	-- root search depth
          absdp :: !Int,	-- absolute depth (root = 0)
          expnt :: !NodeType,	-- expected node type
          usedext :: !Int,	-- used extension
          stats :: SStats	-- search statistics
      } deriving Show

-- This is a state which reflects the status of alpha beta in a node while going through the edges
data NodeState e s
    = NSt {
          forpv :: !Bool,	-- still searching for PV?
          cursc :: Path e s,	-- current alpha value (now plus path & depth)
          movno :: !Int,	-- current move number
          pvsl  :: [Pvsl e s],	-- principal variation list (at root) with node statistics
          weak  :: !Bool,	-- to recognize all nodes
          killer :: Killer e (Path e s), -- the current killer moves
          pvcont :: Seq e	-- a pv continuation from the previous iteration, if available
      }

noMove :: Alt e -> Bool
noMove (Alt es) = null es

nullSeq :: Seq e -> Bool
nullSeq (Seq es) = null es

{-# INLINE checkMe #-}
checkMe :: Node m e s => Path e s -> String -> Search m e s ()
checkMe p pula
    | Just (e1, e2) <- first2Edges p = do
        is <- lift $ inSeq e1 e2
        if is then return () else lift $ stop e1 e2
    | otherwise = return ()
    where stop e1 e2 = informStr (pula ++ " ---> " ++ show e1 ++ ", " ++ show e2)
                           >> informStr undefined
          first2Edges (Path { pathMoves = Seq (e1:e2:_) }) = Just (e1, e2)
          first2Edges _ = Nothing

pvsInit :: Node m e s => PVState e s
pvsInit = PVState { ronly = pvro0, draft = 0, absdp = 0,
                    expnt = PVNode, usedext = 0, stats = stt0 }
nst0 :: Node m e s => NodeState e s
nst0 = NSt { forpv = True, cursc = 0, movno = 1, weak = True,
             killer = NoKiller, pvsl = [], pvcont = Seq [] }
stt0 = SStats { sNodes = 0, sNodesQS = 0, sCuts = 0, sCutMovNo = 0, sRese = 0,
                sNulWind = 0, sRetr = 0, sRSuc = 0 }

pvro0 = PVReadOnly { school = False }
pvro1 = PVReadOnly { school = True }

alphaBeta :: Node m e s => ABControl e s -> m (s, [e], [e])
alphaBeta abc = do
    let !d = maxdepth abc
        rmvs = Alt $ rootmvs abc
        lpv  = Seq $ lastpv abc
        searchReduced a b = pvRootSearch a      b     d lpv rmvs True
        searchLow       b = pvRootSearch alpha0 b     d lpv rmvs True
        searchHigh    a   = pvRootSearch a      beta0 d lpv rmvs True
        searchFull        = pvRootSearch alpha0 beta0 d lpv rmvs False	-- ???
        -- pvs0 = if learnev abc then pvsInit { ronly = pvro1 } else pvsInit
        pvs0 = pvsInit
    r <- if useAspirWin
         then case lastscore abc of
             Just sp -> do
                let !alpha1 = sp - window abc
                    !beta1  = sp + window abc
                informStr $ "+++ Start search with d = " ++ show d
                              ++ " a = " ++ show alpha1
                              ++ " b = " ++ show beta1
                r1@((s1, es1, _), _) <- runSearch (searchReduced alpha1 beta1) pvs0
                if s1 > alpha1 && s1 < beta1 && not (nullSeq es1)
                    then return r1
                    else runSearch searchFull pvs0
{--
                    else if s1 >= beta1
                        then do
                            informStr $ "*** Research high with d = " ++ show d
                              ++ " a = " ++ show alpha1
                            runSearch (searchHigh alpha1) pvs0
                        else do
                            informStr $ "*** Research low with d = " ++ show d
                              ++ " b = " ++ show beta1
                            runSearch (searchLow   beta1) pvs0
--}
             Nothing -> do
                informStr $ "+++ Start search with d = " ++ show d ++ " (full)"
                runSearch searchFull pvs0
         else runSearch searchFull pvs0
    return $ case r of ((s, Seq path, Alt rmvs), _) -> (s, path, rmvs)

-- Root PV Search
pvRootSearch :: Node m e s => s -> s -> Int -> Seq e -> Alt e -> Bool
             -> Search m e s (s, Seq e, Alt e)
pvRootSearch a b d _ _ _ | d <= 0 = do	-- this part is only for eval learning
    v <- pvQSearch a b 0		-- in normal play always d >= 1
    return (v, Seq [], Alt [])
pvRootSearch a b d lastpath rmvs aspir = do
    modify $ \s -> s { draft = d }
    -- Root is pv node, cannot fail low, except when aspiration fails!
    edges <- if null (unalt rmvs)
                then genAndSort lastpath NoKiller d True
                else if null (unseq lastpath)
                        then return rmvs
                        else do
                           let !lm = head (unseq lastpath)
                           return $ Alt $ lm : delete lm (unalt rmvs)
    -- lift $ informStr $ "Root moves: " ++ show edges
    -- pvcont is the pv continuation from the last iteration
    let !pvc  = if nullSeq lastpath then lastpath else Seq $ tail $ unseq lastpath
        !nsti = nst0 { cursc = pathFromScore a, pvcont = pvc }
    nstf <- pvLoop (pvInnerRoot (pathFromScore b) d) nsti edges
    rsr <- if weak nstf		-- failed low
              then do
                 when (not aspir) $ do
                     s <- get
                     lift $ logmes $ "Failed low at root! Status: " ++ show s
                 return (a, Seq [], edges)	-- just to permit aspiration to retry
              else do
                 (s, p) <- lift $ choose $ sortBy (comparing fstdesc)
                                         $ map (\(Pvsl p _ _) -> (pathScore p, unseq $ pathMoves p))
                                         $ filter pvGood $ pvsl nstf
                 when (d < depthForCM) $ informBest s d p
                 let !best = head p
                     !xrmvs = Alt $ best : delete best (unalt edges)	-- best on top
                 return (s, Seq p, xrmvs)
    reportStats
    return rsr
    where fstdesc (a, _) = -a

-- This is the inner loop of the PV search of the root, executed at root once per possible move
-- See the parameter
-- Returns: ...flag if it was a beta cut and new status
-- {-# SPECIALIZE pvInnerRoot :: Node m e Int => Int -> Int -> [e] -> e -> Search m e Int (Bool, [e]) #-}
pvInnerRoot :: Node m e s
            => Path e s	-- current beta
            -> Int	-- current search depth
            -> NodeState e s	-- node status
            -> e	-- move to search
            -> Search m e s (Bool, NodeState e s)
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
             Final sco  -> return $ pathFromScore sco
    -- checkMe s "pvInnerRoot 2"
    -- undo the move
    lift $ undoEdge e
    modify $ \s -> s { absdp = absdp old, usedext = usedext old }
    let s' = nextlev (addToPath e s)
    -- -- checkMe s' "pvInnerRoot 3"
    -- pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
    checkFailOrPVRoot (stats old) b d e s' nst

-- {-# SPECIALIZE pvInnerRootExten :: Node m e Int => Int -> Int -> Bool -> Int -> [e] -> Search m e Int (Int, [e]) #-}
pvInnerRootExten :: Node m e s => Path e s -> Int -> Bool -> Int -> NodeState e s -> Search m e s (Path e s)
pvInnerRootExten b d spec exd nst = do
    -- pindent $ "depth = " ++ show d
    -- checkMe b "pvInnerRootExten 1"
    old <- get
    exd' <- reserveExtension (usedext old) exd
    -- pindent $ "exd' = " ++ show exd'
    pvpath <- if not . nullSeq $ pvcont nst then return (pvcont nst) else bestMoveFromHash
    tact <- lift tactical
    let !reduce = lmrActive && not (tact || spec || exd > 0 || d < lmrMinDRed)
        !(d', reduced) = nextDepth (d+exd') (draft old) (movno nst) reduce (forpv nst)
    modify $ \s -> s { expnt = PVNode }		-- why??
    -- when (d' < d-1) $ pindent $ "d' = " ++ show d'
    let !a = cursc nst
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
                 reSearch
                 -- pindent $ "Research! (" ++ show s1 ++ ")"
                 if reduced
                    then do	-- re-search with no reduce for root moves
                      let d''= fst $! nextDepth (d+exd') (draft old) (movno nst) False (forpv nst)
                      pvSearch nst (-b) (-a) d'' pvpath nulMoves
                    else pvSearch nst (-b) (-a) d' pvpath nulMoves
              else return s1

-- {-# SPECIALIZE checkFailOrPVRoot :: Node m e Int => PVState e Int -> Int -> Int -> Int -> e -> Int -> [e]
--                   -> Search m e Int (Bool, [e]) #-}
checkFailOrPVRoot :: Node m e s => SStats -> Path e s -> Int -> e -> Path e s
                  -> NodeState e s -> Search m e s (Bool, NodeState e s)
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
        xpvslg = insertToPvs d pvg (pvsl nst)	-- the good
        xpvslb = insertToPvs d pvb (pvsl nst)	-- the bad
    -- logmes $ "*** to pvsl: " ++ show xpvsl
    inschool <- gets $ school . ronly
    if d == 1	-- for depth 1 search we search all exact
       then do
            let typ = 2
            when (d >= minToStore) $ lift $ store d typ (pathScore s) e nodes
            -- when inschool $ do
            --     s0 <- pvQSearch a b 0
            --     lift $ learn d typ s s0
            let nst1 = if s > a
                          then nst { cursc = s, forpv = False, weak = False }
                          else nst
            return (False, nst1 {movno = mn + 1, pvsl = xpvslg, pvcont = Seq []})
       else if s >= b
               then do	-- what when a root move fails high?
                    let typ = 1	-- best move is e and is beta cut (score is lower limit)
                    when (d >= minToStore) $ lift $ store d typ (pathScore s) e nodes
                    -- when inschool $ do
                    --     s0 <- pvQSearch a b 0
                    --     lift $ learn d typ b s0
                    lift $ betaMove True d (absdp sst) e
                    put sst { stats = statCut (stats sst) mn }
                    let nst1 = nst { cursc = combinePath s b, weak = False,
                                     pvsl = xpvslg, pvcont = Seq [] }
                    -- lift $ logmes $ "Root move " ++ show e ++ " failed high: " ++ show s
                    -- lift $ informStr $ "Cut (" ++ show b ++ "): " ++ show np
                    return (True, nst1)
               else if s > a
                    then do
                         informBest (pathScore s) (draft sst) (unseq $ pathMoves s)
                         let typ = 2	-- best move so far (score is exact)
                         when (d >= minToStore) $ lift $ store d typ (pathScore s) e nodes
                         -- when inschool $ do
                         --     s0 <- pvQSearch a b 0
                         --     lift $ learn d typ s s0
                         let nst1 = nst { cursc = s, forpv = False, movno = mn + 1,
                                          weak = False, pvsl = xpvslg, pvcont = Seq [] }
                         -- lift $ logmes $ "Root move " ++ show e ++ " improves alpha: " ++ show s
                         -- lift $ informStr $ "Better (" ++ show s ++ "):" ++ show np
                         return (False, nst1)
                    else do
                         -- when in a cut node and the move dissapointed - negative history
                         when (useNegHist && forpv nst && a == b - 1 && mn <= negHistMNo)
                              $ lift $ betaMove False d (absdp sst) e
                         let nst1 = nst { movno = mn + 1, pvsl = xpvslb, pvcont = Seq [] }
                         return (False, nst1)

-- {-# SPECIALIZE insertToPvs :: Score Int => Int -> Pvsl e Int -> [Pvsl e Int] -> [Pvsl e Int] #-}
insertToPvs :: (Show e, Edge e, Score s) => Int -> Pvsl e s -> [Pvsl e s] -> [Pvsl e s]
insertToPvs _ p [] = [p]
insertToPvs d p ps@(q:qs)
    -- | pvGood p && not (pvGood q) || betters || equals && bettern = p : ps
    -- | pvGood p && not (pvGood q) || bettern || equaln && betters = p : ps
    | d == 1 && (betters || equals) = p : ps
    | pmate && not qmate            = p : ps
    | not pmate && qmate            = q : insertToPvs d p qs
    | pmate && betters              = p : ps
    | bettern || equaln && betters  = p : ps
    -- | pvGood p && not (pvGood q) || betters = p : ps
    | otherwise                    = q : insertToPvs d p qs
    where betters = pvPath p >  pvPath q
          equals  = pvPath p == pvPath q
          equaln  = pvNodes p == pvNodes q
          bettern = pvNodes p > pvNodes q
          pmate   = nearmate $ pvPath p
          qmate   = nearmate $ pvPath q

-- PV Search
-- {-# SPECIALIZE pvSearch :: Node m e Int => Int -> Int -> Int -> [e] -> Int
--                         -> Search m e Int (Int, [e]) #-}
pvSearch :: Node m e s => NodeState e s -> Path e s -> Path e s -> Int -> Seq e -> Int
                       -> Search m e s (Path e s)
pvSearch _ a b d _ _ | d <= 0 = do
    -- checkMe a "pvSearch 1"
    -- checkMe b "pvSearch 2"
    v <- pvQSearch (pathScore a) (pathScore b) 0
    when debug $ lift $ logmes $ "<-- pvSearch: reach depth 0, return " ++ show v
    -- let !v' = if v <= a then a else if v > b then b else v
    -- pindent $ "<> " ++ show v
    return $ pathFromScore v
pvSearch nst !a !b d lastpath lastnull = do
    -- pindent $ "=> " ++ show a ++ ", " ++ show b
    -- checkMe a "pvSearch 3"
    -- checkMe b "pvSearch 4"
    nmfail <- nullEdgeFailsHigh nst b d lastnull
    if nmfail
       -- then pindent ("<= " ++ show b) >> return (onlyScore b)
       then return $ onlyScore b
       else do
          ---- Here: we dont know where to put the killer
          ---- so we deactivate them for now
          let kill = killer nst
          edges <- genAndSort lastpath kill d (forpv nst)
          if noMove edges
             then do
                  v <- lift staticVal
                  -- pindent ("<= " ++ show v)
                  return $ pathFromScore v
             else do
                  -- Loop thru the moves
                  -- modify $ \s -> s { forpv = True, cursc = a, movno = 1,
                  --                    killer = NoKiller, weak = True }
                  --                    -- killer = NoKiller, weak = True, expnt = PVNode }
                  let !pvpath = if nullSeq lastpath then Seq [] else Seq $ tail $ unseq lastpath
                      !nsti = nst0 { cursc = a, pvcont = pvpath }
                  nodes0 <- gets stats >>= return . sNodes
                  nstf <- pvLoop (pvInnerLoop b d) nsti edges
                  nodes1 <- gets stats >>= return . sNodes
                  -- let !kill1  = pushKiller (head $ pathMoves s) s kill
                  s <- if weak nstf
                          then do
                              inschool <- gets $ school . ronly
                              -- when inschool $ do
                              --     s0 <- pvQSearch a b 0
                              --     lift $ learn d typ s s0
                              let s = cursc nstf
                              when (d >= minToStore) $ do
                                  let typ = 0
                                      !deltan = nodes1 - nodes0
                                  -- store as upper score - move does not matter - tricky here!
                                  lift $ store d typ (pathScore s) (head $ unalt edges) deltan
                              return $ onlyScore s
                          -- else modify $ \st -> st { killer = pushKiller (head p) s kill }
                          else return (cursc nstf)	-- modify $ \st -> st { killer = kill1 }
                  -- checkMe s "pvSearch 5"
                  -- pindent $ "<= " ++ show s
                  return s

-- {-# SPECIALIZE nullEdgeFailsHigh :: Node m e Int => Int -> Int -> Int -> Int -> Search m e Int Bool #-}
nullEdgeFailsHigh :: Node m e s => NodeState e s -> Path e s -> Int -> Int -> Search m e s Bool
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
               let !nmb = if nulSubAct && not inschool then b - nulSubmrg else b
                   !d1  = d - 1 - nulRedux
                   !lastnull1 = lastnull - 1
               val <- pvSearch nst (-nmb) (-nmb + nulMargin) d1 (Seq []) lastnull1
               lift nullEdge	-- undo null move
               return $! (-val) >= nmb

-- This is the inner loop of the PV search, executed at every level (except root) once per possible move
-- See the parameter
-- Returns: flag if it was a beta cut and new status
-- {-# SPECIALIZE pvInnerLoop :: Node m e Int => Int -> Int -> [e] -> e -> Search m e Int (Bool, [e]) #-}
pvInnerLoop :: Node m e s
            => Path e s	-- current beta
            -> Int	-- current search depth
            -> NodeState e s	-- node status
            -> e	-- move to search
            -> Search m e s (Bool, NodeState e s)
pvInnerLoop b d nst e = do
    -- checkMe b "pvInnerLoop 1"
    old <- get
    -- pindent $ "-> " ++ show e
    exd <- lift $ doEdge e False	-- do the move
    newNode
    modify $ \s -> s { absdp = absdp s + 1 }
    s <- case exd of
             Exten exd' -> pvInnerLoopExten b d (special e) exd' nst
             Final sco  -> return $ pathFromScore sco
    -- checkMe s "pvInnerLoop 2"
    lift $ undoEdge e	-- undo the move
    modify $ \s -> s { absdp = absdp old, usedext = usedext old }
    let s' = nextlev (addToPath e s)
    -- -- checkMe s' "pvInnerLoop 3"
    -- pindent $ "<- " ++ show e ++ " (" ++ show s' ++ ")"
    checkFailOrPVLoop (stats old) b d e s' nst

reserveExtension uex exd = do
    if uex >= maxDepthExt || exd == 0
       then return 0
       else do
            modify $ \s -> s { usedext = usedext s + exd }
            return exd

-- {-# SPECIALIZE pvInnerLoopExten :: Node m e Int => Int -> Int -> Bool -> Int -> [e] -> Search m e Int (Int, [e]) #-}
pvInnerLoopExten :: Node m e s => Path e s -> Int -> Bool -> Int -> NodeState e s
                 -> Search m e s (Path e s)
pvInnerLoopExten b d spec exd nst = do
    -- checkMe b "pvInnerLoopExten 1"
    old <- get
    tact <- lift tactical
    exd' <- reserveExtension (usedext old) exd
    let !mn = movno nst
        !a = cursc nst
        -- late move reduction
        !reduce = lmrActive && not (nearmate a || tact || spec || exd > 0 || d < lmrMinDRed)
        !(d', reduced) = nextDepth (d+exd') (draft old) mn reduce (forpv nst)
    -- checkMe a "pvInnerLoopExten 2"
    if forpv nst 	-- && a < b - 1	-- to get really PV (otherwise cut/all)
       then do
          pvpath <- if nullSeq (pvcont nst) then bestMoveFromHash else return (pvcont nst)
          pvSearch nst (-b) (-a) d' pvpath nulMoves
       else do
          (hdeep, tp, hscore, e', nodes)
              <- if d >= minToRetr
                    then reTrieve >> lift retrieve
                    else return (-1, 0, 0, undefined, 0)
          -- let pvpath = if null lastpath
          --           then if hdeep > 0 && tp > 0 then [e'] else []
          --           else lastpath
          let !pvpath = if hdeep > 0 && tp > 0 then Seq [e'] else (pvcont nst)
          --1-- let !pvpath = if hdeep > 0 && tp > 0 then Seq [] else (pvcont nst)
              -- !hs = nextlev hscore
              ttpath = Path { pathScore = hscore, pathDepth = hdeep, pathMoves = Seq [e'] }
              --2-- ttpath = Path { pathScore = hscore, pathDepth = hdeep, pathMoves = Seq [] }
              hs = - ttpath
          if hdeep >= d && (tp == 2 || tp == 1 && hs > a || tp == 0 && hs <= a)
             then reSucc nodes >> return ttpath
             else do
                 -- futility pruning
                 inschool <- gets $ school . ronly
                 (prune, v) <- if not futilActive || tact || inschool
                                  -- don't prune when tactical or in learning
                                  then return (False, 0)
                                  else isPruneFutil d (-b) (-a)
                 -- -- checkMe v "pvInnerLoopExten 3"
                 if prune
                    then return v	-- we will fail low or high
                    else do
                       nulWind
                       s1 <- pvSearch nst (-a-1) (-a) d' pvpath nulMoves
                       -- -- checkMe s1 "pvInnerLoopExten 4"
                       if -s1 > a -- we need re-search
                          then do
                            reSearch
                            if reduced && d >= lmrMinDFRes
                               then do	-- re-search with no reduce is expensive!
                                  let !d'' = fst $ nextDepth (d+exd') (draft old) mn False (forpv nst)
                                  pvSearch nst (-b) (-a) d'' pvpath nulMoves
                               else pvSearch nst (-b) (-a) d' pvpath nulMoves
                          else return s1

-- {-# SPECIALIZE checkFailOrPVLoop :: Node m e Int => PVState e Int -> Int -> Int -> Int -> e -> Int -> [e] -> Search m e Int (Bool, [e]) #-}
checkFailOrPVLoop :: Node m e s => SStats -> Path e s -> Int -> e -> Path e s
                  -> NodeState e s -> Search m e s (Bool, NodeState e s)
checkFailOrPVLoop xstats b d e s nst = do
    -- checkMe b "checkFailOrPVLoop 1"
    -- checkMe s "checkFailOrPVLoop 2"
    sst <- get
    let !mn = movno nst
        !a  = cursc nst
        !nodes0 = sNodes xstats
        !nodes1 = sNodes $ stats sst
        !nodes  = nodes1 - nodes0
    inschool <- gets $ school . ronly
    -- checkMe a "checkFailOrPVLoop 3"
    if s >= b
       then do
            let typ = 1	-- best move is e and is beta cut (score is lower limit)
            when (d >= minToStore) $ lift $ store d typ (pathScore s) e nodes
            lift $ betaMove True d (absdp sst) e -- anounce a beta move (for example, update history)
            -- when inschool $ do
            --     s0 <- pvQSearch a b 0
            --     lift $ learn d typ b s0
            -- when debug $ logmes $ "<-- pvInner: beta cut: " ++ show s ++ ", return " ++ show b
            put sst { stats = statCut (stats sst) mn }
            let nst1 = nst { cursc = combinePath s b, weak = False, pvcont = Seq [] }
            -- lift $ informStr $ "Cut (" ++ show b ++ "): " ++ show np
            return (True, nst1)
       else if s > a
          then do
              let typ = 2	-- score is exact
              when (forpv nst || d >= minToStore) $ lift $ store d typ (pathScore s) e nodes
              -- when debug $ logmes $ "<-- pvInner - new a: " ++ show s
              -- when inschool $ do
              --     s0 <- pvQSearch a b 0
              --     lift $ learn d typ s s0
              let !mn1 = mn + 1
              let nst1 = nst { cursc = s, forpv = False, movno = mn1, weak = False, pvcont = Seq [] }
              -- lift $ informStr $ "Better (" ++ show s ++ "): " ++ show np
              return (False, nst1)
          else do
              -- when in a cut node and the move dissapointed - negative history
              when (useNegHist && forpv nst && a == b - 1 && mn <= negHistMNo)
                   $ lift $ betaMove False d (absdp sst) e
              let !mn1 = mn + 1
              let nst1 = nst { movno = mn1, pvcont = Seq [] }
              return (False, nst1)

-- We don't sort the moves here, they have to come sorted from genEdges
-- But we consider the best moves first (best from previous iteration, killers)
{-# INLINE genAndSort #-}
genAndSort :: Node m e s => Seq e -> Killer e (Path e s) -> Int -> Bool -> Search m e s (Alt e)
genAndSort lastpath kill d pv = do
    kl <- liftM Alt $ lift (filterM legalEdge $ unalt $ killerToList kill)
    adp <- gets absdp 
    lift $ liftM (bestFirst lastpath kl . Alt) (genEdges d adp pv')
    where pv' = pv || not (nullSeq lastpath)

-- Late Move Reduction
{-# INLINE nextDepth #-}
nextDepth :: Int -> Int -> Int -> Bool -> Bool -> (Int, Bool)
nextDepth d rd w lmr pv = (max 0 nd, reduced)
    where !nd = if lmr then d - k else d - 1
          idx = (min lmrMaxDepth d, min lmrMaxWidth w)
          k  = if pv then lmrReducePv ! idx else lmrReduceArr ! idx
          -- dg = (d + deGran - 1) `div` deGran
          !reduced = nd < d - 1

-- This is a kind of monadic fold optimized for (beta) cut
{-# INLINE pvLoop #-}
pvLoop :: Monad m => (s -> e -> m (Bool, s)) -> s -> Alt e -> m s
pvLoop _ s (Alt [])     = return s
pvLoop f s (Alt (e:es)) = do
    (cut, s') <- f s e
    if cut then return s'
           else pvLoop f s' $ Alt es

-- {-# SPECIALIZE isPruneFutil :: Node m e Int => Int -> Int -> Int -> Search m e Int (Bool, Int) #-}
isPruneFutil :: Node m e s => Int -> Path e s -> Path e s -> Search m e s (Bool, Path e s)
isPruneFutil d a b
    | d > maxFutilDepth = return (False, 0)
    | otherwise = do
        -- checkMe a "isPruneFutere 1"
        -- checkMe b "isPruneFutere 2"
        let !margin = pathFromScore $ futilMargins ! d
        -- v <- lift materVal	-- can we do here direct static evaluation?
        v <- liftM pathFromScore $ pvQSearch (pathScore a) (pathScore b) 0
        if v < a && v + margin <= a
           then return (True, onlyScore a)
           else if v > b && v - margin >= b
                then return (True, onlyScore b)
                else return (False, 0)

-- PV Quiescent Search
{-# SPECIALIZE pvQSearch :: Node m e Int => Int -> Int -> Int -> Search m e Int Int #-}
pvQSearch :: Node m e s => s -> s -> Int -> Search m e s s
-- pvQSearch a b c | c > qsMaxChess = lift staticVal  -- limit the no of chess in qs path
-- pvQSearch a b c | c > qsMaxChess = return 0  -- TEST: consider here endless loop
pvQSearch a b c = do				   -- to avoid endless loops
    -- qindent $ "=> " ++ show a ++ ", " ++ show b
    stp <- lift staticVal				-- until we can recognize repetition
    tact <- lift tactical
    if tact
       then do
           edges <- liftM Alt $ lift $ genEdges 0 0 False
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
                          let !esc = length $ take 3 $ unalt edges
                              !nc = c + esc - 2
                              !a' = if stp > a then stp else a
                          s <- pvLoop (pvQInnerLoop b nc) a' edges
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
                                 s <- pvLoop (pvQInnerLoop b c) a' edges
                                 -- qindent $ "<= " ++ show s
                                 return s

-- {-# INLINE pvQInnerLoop #-}
{-# SPECIALIZE pvQInnerLoop :: Node m e Int => Int -> Int -> Int -> e -> Search m e Int (Bool, Int) #-}
pvQInnerLoop :: Node m e s => s -> Int -> s -> e -> Search m e s (Bool, s)
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

{-# SPECIALIZE bestMoveFromHash :: Node m e Int => Search m e Int (Seq e) #-}
bestMoveFromHash :: Node m e s => Search m e s (Seq e)
bestMoveFromHash = do
    reTrieve
    (hdeep, tp, _, e, _) <- lift retrieve
    when (hdeep > 0) $ reSucc 1		-- here we save just move generation
    -- return $! if hdeep > 0 && tp > 0 then [e] else []
    return $! Seq [ e | hdeep > 0 && tp > 0 ]
    --3-- return $! Seq []

{-# INLINE reportStats #-}
reportStats :: Node m e s => Search m e s ()
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

-- {-# INLINE newNode #-}
{-# SPECIALIZE newNode :: Node m e Int => Search m e Int () #-}
newNode :: Node m e s => Search m e s ()
newNode = do
    xst <- gets stats
    let !n1 = sNodes xst + 1
    modify $ \s -> s { stats = xst { sNodes = n1 }}

-- {-# INLINE newNodeQS #-}
{-# SPECIALIZE newNodeQS :: Node m e Int => Search m e Int () #-}
newNodeQS :: Node m e s => Search m e s ()
newNodeQS = do
    xst <- gets stats
    let !n1 = sNodes xst + 1
        !n2 = sNodesQS xst + 1
    modify $ \s -> s { stats = xst { sNodes = n1, sNodesQS = n2 }} 

-- {-# INLINE nulWind #-}
{-# SPECIALIZE nulWind :: Node m e Int => Search m e Int () #-}
nulWind :: Node m e s => Search m e s ()
nulWind = do
    xst <- gets stats
    let !n1 = sNulWind xst + 1
    modify $ \s -> s { stats = xst { sNulWind = n1 }}

-- {-# INLINE reSearch #-}
{-# SPECIALIZE reSearch :: Node m e Int => Search m e Int () #-}
reSearch :: Node m e s => Search m e s ()
reSearch = do
    xst <- gets stats
    let !n1 = sRese xst + 1
    modify $ \s -> s { stats = xst { sRese = n1 }}

{-# SPECIALIZE reTrieve :: Node m e Int => Search m e Int () #-}
reTrieve :: Node m e s => Search m e s ()
reTrieve = do
    xst <- gets stats
    let !n1 = sRetr xst + 1
    modify $ \s -> s { stats = xst { sRetr = n1 }}

{-# SPECIALIZE reSucc :: Node m e Int => Int -> Search m e Int () #-}
reSucc :: Node m e s => Int -> Search m e s ()
reSucc n = do
    xst <- gets stats
    let !n1 = sRSuc xst + n
    modify $ \s -> s { stats = xst { sRSuc = n1 }}

indentActive :: Node m e s => String -> Search m e s ()
indentActive s = do
    ad <- gets absdp
    lift $ informStr $ take ad (repeat ' ') ++ s

indentPassive :: Node m e s => String -> Search m e s ()
indentPassive _ = return ()

pindent, qindent :: Node m e s => String -> Search m e s ()
pindent = indentPassive
qindent = indentPassive

bestFirst :: Eq e => Seq e -> Alt e -> Alt e -> Alt e
bestFirst path kl es
    | null (unseq path) = Alt $ kll ++ delall esl kll
    | otherwise         = Alt $ e : kle ++ delall esl (e : kll)
    where kle = delete e kll
          delall = foldr delete
          e = head . unseq $ path
          kll = unalt kl
          esl = unalt es

statCut :: SStats -> Int -> SStats
statCut s n = s { sCuts = sCuts s + 1, sCutMovNo = sCutMovNo s + n }

pushKiller :: (Eq e, Show e, Ord s, Show s) => e -> s -> Killer e s -> Killer e s
pushKiller e s NoKiller = OneKiller e s
pushKiller e s (OneKiller e1 s1) = if s > s1
                                      then TwoKillers e s e1 s1
                                      else TwoKillers e1 s1 e s
pushKiller e s tk@(TwoKillers e1 s1 e2 s2)
    | s > s1    = TwoKillers e s e1 s1
    | s > s2    = TwoKillers e1 s1 e s
    | otherwise = tk

killerToList :: (Show e, Show s) => Killer e s -> Alt e
killerToList NoKiller = Alt []
killerToList (OneKiller e _) = Alt [e]
killerToList (TwoKillers e1 _ e2 _) = Alt [e1, e2]

--- Communication to the outside - some convenience functions ---

informBM a b c d = inform (BestMv a b c d)

informCM a b = inform (CurrMv a b)

informStr s = inform (InfoStr s)

logmes s = inform (LogMes s)

informBest :: Node m e s => s -> Int -> [e] -> Search m e s ()
informBest s d es = do
    n <- lift curNodes
    lift $ informBM s d n es
