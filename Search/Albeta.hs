{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts #-}

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
depthForCM  = 8 -- from this depth inform current move
minToStore  = 1 -- minimum remaining depth to store the position in hash
minToRetr   = 1 -- minimum remaining depth to retrieve
maxDepthExt = 3 -- maximum depth extension
useNegHist  = False	-- when not cutting - negative history
negHistMNo  = 1		-- how many moves get negative history

-- Parameters for late move reduction:
lmrMinDFRes = 8		-- minimum depth for full research when failed high in null window
lmrMinDRed  = 2		-- minimum reduced depth
lmrMaxDepth = 15
lmrMaxWidth = 63
lmrPv     = 3.8
lmrRest   = 2.2
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
maxFutilDepth = 3
futilMargins :: Score s => Array Int s
-- futilMargins = array (1, 3) [ (1, 400), (2, 650), (3, 1200) ]
futilMargins = array (1, 3) [ (1, 500), (2, 750), (3, 1200) ]
-- futilMargins = array (1, 3) [ (1, 600), (2, 900), (3, 1200) ]

-- Parameters for quiescent search:
qsBetaCut  = False	-- use beta cut in QS?
qsDeltaCut = False	-- use delta prune in QS?
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

data Pvsl e s = Pvsl {
        pvPath :: [e],		-- pv path
        pvScore :: !s,		-- last score
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
          forpv :: !Bool,	-- still searching for PV?
          cursc :: !s,		-- current alpha value
          path  :: [e],		-- best path so far
          movno :: !Int,	-- current move number
          pvsl  :: [Pvsl e s],	-- principal variation list (at root) with node statistics
          expnt :: !NodeType,	-- expected node type
          weak  :: !Bool,	-- to recognize all nodes
          usedext :: !Int,	-- used extension
          killer :: Killer e s, -- the current killer moves
          stats :: SStats	-- search statistics
      } deriving Show

pvsInit :: Node m e s => PVState e s
pvsInit = PVState { ronly = pvro0, draft = 0, absdp = 0, forpv = True, cursc = 0, path = [],
                    movno = 1, pvsl = [], expnt = PVNode, weak = True, usedext = 0,
                    killer = NoKiller, stats = stt0 }
stt0 = SStats { sNodes = 0, sNodesQS = 0, sCuts = 0, sCutMovNo = 0, sRese = 0,
                sNulWind = 0, sRetr = 0, sRSuc = 0 }

pvro0 = PVReadOnly { school = False }
pvro1 = PVReadOnly { school = True }

alphaBeta :: Node m e s => ABControl e s -> m (s, [e], [e])
alphaBeta abc = do
    let !d = maxdepth abc
        searchFull        = pvRootSearch alpha0 beta0 d (rootmvs abc) (lastpv abc) False
        searchReduced a b = pvRootSearch a      b     d (rootmvs abc) (lastpv abc) True
        pvs0 = if learnev abc then pvsInit { ronly = pvro1 } else pvsInit
    r <- if useAspirWin
         then case lastscore abc of
             Just sp -> do
                let !alpha1 = sp - window abc
                    !beta1  = sp + window abc
                r1@((s1, es1, _), _) <- runSearch (searchReduced alpha1 beta1) pvs0
                if s1 > alpha1 && s1 < beta1 && not (null es1)
                    then return r1
                    else runSearch searchFull pvs0
             Nothing -> runSearch searchFull pvs0
         else runSearch searchFull pvs0
    return $ fst r

-- Root PV Search
pvRootSearch :: Node m e s => s -> s -> Int -> [e] -> [e] -> Bool
             -> Search m e s (s, [e], [e])
pvRootSearch a b d _ _ _ | d <= 0 = do	-- this part is only for eval learning
    v <- pvQSearch a b 0		-- in normal play always d >= 1
    return (v, [], [])
pvRootSearch a b d rmvs lastpath aspir = do
    modify $ \s -> s { draft = d, cursc = a }
    -- Root is pv node, cannot fail low, except when aspiration fails!
    edges <- if null rmvs
                then genAndSort [] NoKiller d True
                else if null lastpath
                        then return rmvs
                        else do
                           let !lm = head lastpath
                           return $ lm : delete lm rmvs
    -- pvcont is the pv continuation from the last iteration
    let !pvcont = if null lastpath then [] else tail lastpath
{--
    if aspir
       then do
           -- When aspiration, we check only the best move, if it fails low, then we return
           -- otherwise we try the rest
           let !mv = if null edges then error "pvRootSearch" else head edges
           pvLoop (pvInnerRoot b d) pvcont [mv]
           failedLow <- gets weak
           if failedLow
              then return []
              else do
                   modify $ \s -> s { forpv = False }
                   pvLoop (pvInnerRoot b d) [] (tail edges)
       else pvLoop (pvInnerRoot b d) pvcont edges
--}
    pvLoop (pvInnerRoot b d) pvcont edges
    failedLow <- gets weak
    rsr <- if failedLow
              then do
                 when (not aspir) $ do
                     s <- get
                     lift $ logmes $ "Failed low at root! Status: " ++ show s
                 return (a, [], edges)	-- just to permit aspiration to retry
              else do
                 r <- get
                 (s, p) <- lift $ choose $ sortBy (comparing fstdesc)
                                         $ map (\(Pvsl p s _ _) -> (s, p))
                                         $ filter pvGood $ pvsl r
                 when (d < depthForCM) $ informBest s d p
                 -- let xrmvs = map (head . pvPath) (pvsl r)	-- when sorting from insert
                 let !xrmvs = best : delete best edges		-- best on top
                     !best = head p
                 return (s, p, xrmvs)
    reportStats
    return rsr
    where fstdesc (a, _) = -a

-- This is the inner loop of the PV search of the root, executed at root once per possible move
-- See the parameter
-- Returns: ...flag if it was a beta cut and new status
{-# SPECIALIZE pvInnerRoot :: Node m e Int => Int -> Int -> [e] -> e -> Search m e Int (Bool, [e]) #-}
pvInnerRoot :: Node m e s
            => s	-- current beta
            -> Int	-- current search depth
            -> [e]	-- "status" is hier the pv continuation
            -> e	-- move to search
            -> Search m e s (Bool, [e])
pvInnerRoot b d lastpath e = do
    -- when debug $ logmes $ "--> pvInner: b d old: " ++ show b ++ ", " ++ show d ++ ", " ++ show old
    old <- get
    let !a = cursc old
        !mn = movno old
    when (draft old >= depthForCM) $ lift $ informCM e mn
    -- lift $ logmes $ "Search root move " ++ show e ++ " a = " ++ show a ++ " b = " ++ show b
    -- do the move
    exd <- lift $ doEdge e False
    newNode
    modify $ \s -> s { absdp = absdp s + 1 }
    (s, p) <- case exd of
                Exten exd' -> pvInnerRootExten b d (special e) exd' lastpath
                Final sco  -> return (sco, [])
    -- undo the move
    lift $ undoEdge e
    modify $ \s -> s { absdp = absdp s - 1 }
    checkFailOrPVRoot old a b d e s p

{-# SPECIALIZE pvInnerRootExten :: Node m e Int => Int -> Int -> Bool -> Int -> [e] -> Search m e Int (Int, [e]) #-}
pvInnerRootExten :: Node m e s => s -> Int -> Bool -> Int -> [e] -> Search m e s (s, [e])
pvInnerRootExten b d spec exd lastpath = do
    old <- get
    let !a = cursc old
    exd' <- if usedext old >= maxDepthExt
              then return 0
              else do
                modify $ \s -> s { usedext = usedext s + exd }
                return exd
    pvpath <- if not . null $ lastpath then return lastpath else bestMoveFromHash
    tact <- lift tactical
    let !reduce = not (tact || spec || exd > 0 || d < lmrMinDRed)
        !(d', reduced) = nextDepth (d+exd') (draft old) (movno old) reduce (forpv old)
    modify $ \s -> s { forpv = True, path = [], movno = 1, pvsl = [],
                       weak = True, expnt = PVNode }
    let !b1 = -a
        !a1 = -b
    if forpv old
       then pvSearch a1 b1 d' pvpath nulMoves
       else do
           -- no futility pruning for root moves!
           nulWind
           let !a2 = b1 - 1
           r@(s1, _) <- pvSearch a2 b1 d' pvpath nulMoves
           if (-s1) > a		 -- we didn't fail low, so we need re-search
              then do
                 reSearch
                 if reduced
                    then do	-- re-search with no reduce for root moves
                      let d''= fst $! nextDepth (d+exd') (draft old) (movno old) False (forpv old)
                      pvSearch a1 b1 d'' pvpath nulMoves
                    else pvSearch a1 b1 d' pvpath nulMoves
              else return r

{-# SPECIALIZE checkFailOrPVRoot :: Node m e Int => PVState e Int -> Int -> Int -> Int -> e -> Int -> [e]
                  -> Search m e Int (Bool, [e]) #-}
checkFailOrPVRoot :: Node m e s => PVState e s -> s -> s -> Int -> e -> s -> [e]
                  -> Search m e s (Bool, [e])
checkFailOrPVRoot old a b d e s' p = do
    xstats <- gets stats
    let !mn     = movno old
        !s      = - s'
        !np     = e : p
        -- nodes0 = sNodes $ stats old
        -- nodes1 = sNodes xstats
        !nodes0 = sNodes (stats old) + sRetr (stats old)
        !nodes1 = sNodes xstats + sRetr xstats
        !nodes  = nodes1 - nodes0
        pvg    = Pvsl (e:p) s nodes True	-- the good
        pvb    = Pvsl (e:p) s nodes False	-- the bad
        xpvslg = insertToPvs d pvg (pvsl old)	-- the good
        xpvslb = insertToPvs d pvb (pvsl old)	-- the bad
    -- logmes $ "*** to pvsl: " ++ show xpvsl
    inschool <- gets $ school . ronly
    if d == 1	-- for depth 1 search we search all exact
       then do
            let typ = 2
            when (d >= minToStore) $ lift $ store d typ s e nodes
            when inschool $ do
                s0 <- pvQSearch a b 0
                lift $ learn d typ s s0
            if s > a
               then put old { cursc = nextlev s, path = np, forpv = False,
                      movno = mn + 1, pvsl = xpvslg, weak = False, stats = xstats }
               else put old { movno = mn + 1, pvsl = xpvslg, stats = xstats }
            return (False, [])
       else if s >= b
               then do	-- what is when a root move fails high???
                    let typ = 1	-- best move is e and is beta cut (score is lower limit)
                    when (d >= minToStore) $ lift $ store d typ s e nodes
                    when inschool $ do
                        s0 <- pvQSearch a b 0
                        lift $ learn d typ b s0
                    lift $ betaMove True d (absdp old) e
                    put old { cursc = nextlev b, path = np, pvsl = xpvslg,
                                weak = False, stats = statCut xstats mn }
                    -- lift $ logmes $ "Root move " ++ show e ++ " failed high: " ++ show s
                    return (True, [])
               else if s > a
                    then do
                         informBest s (draft old) np
                         let typ = 2	-- best move so far (score is exact)
                         when (d >= minToStore) $ lift $ store d typ s e nodes
                         when inschool $ do
                             s0 <- pvQSearch a b 0
                             lift $ learn d typ s s0
                         put old { cursc = nextlev s, path = np, forpv = False,
                                   movno = mn + 1, pvsl = xpvslg, weak = False, stats = xstats }
                         -- lift $ logmes $ "Root move " ++ show e ++ " improves alpha: " ++ show s
                         return (False, [])
                    else do
                         -- when in a cut node and the move dissapointed - negative history
                         when (useNegHist && forpv old && a == b - 1 && mn <= negHistMNo)
                              $ lift $ betaMove False d (absdp old) e
                         put old { movno = mn + 1, pvsl = xpvslb, stats = xstats }
                         return (False, [])

{-# SPECIALIZE insertToPvs :: Score Int => Int -> Pvsl e Int -> [Pvsl e Int] -> [Pvsl e Int] #-}
insertToPvs :: Score s => Int -> Pvsl e s -> [Pvsl e s] -> [Pvsl e s]
insertToPvs _ p [] = [p]
insertToPvs d p ps@(q:qs)
    -- | pvGood p && not (pvGood q) || betters || equals && bettern = p : ps
    -- | pvGood p && not (pvGood q) || bettern || equaln && betters = p : ps
    | d == 1 && (betters || equals) = p : ps
    | bettern || equaln && betters = p : ps
    -- | pvGood p && not (pvGood q) || betters = p : ps
    | otherwise                    = q : insertToPvs d p qs
    where betters = pvScore p >  pvScore q
          equals  = pvScore p == pvScore q
          equaln  = pvNodes p == pvNodes q
          bettern = pvNodes p > pvNodes q

-- PV Search
{-# SPECIALIZE pvSearch :: Node m e Int => Int -> Int -> Int -> [e] -> Int
                        -> Search m e Int (Int, [e]) #-}
pvSearch :: Node m e s => s -> s -> Int -> [e] -> Int -> Search m e s (s, [e])
pvSearch a b d _ _ | d <= 0 = do
    v <- pvQSearch a b 0
    when debug $ lift $ logmes $ "<-- pvSearch: reach depth 0, return " ++ show v
    return (v, [])
pvSearch a b d lastpath lastnull = do
    nmfail <- nullEdgeFailsHigh a b d lastnull
    if nmfail
       then return (b, [])
       else do
          kill  <- gets killer
          pv    <- gets forpv
          edges <- genAndSort lastpath kill d pv
          if null edges
             then do
                  v <- lift staticVal
                  return (v, [])
             else do
                  -- Loop thru the moves
                  modify $ \s -> s { forpv = True, cursc = a, path = [], movno = 1,
                                     killer = NoKiller, weak = True }
                                     -- killer = NoKiller, weak = True, expnt = PVNode }
                  let !pvpath = if null lastpath then [] else tail lastpath
                  nodes0 <- gets stats >>= return . sNodes
                  pvLoop (pvInnerLoop b d) pvpath edges
                  nodes1 <- gets stats >>= return . sNodes
                  s   <- gets cursc
                  p   <- gets path
                  low <- gets weak
                  let typ = 0
                      !deltan = nodes1 - nodes0
                      !kill1  = pushKiller (head p) s kill
                  if low
                      then do
                          inschool <- gets $ school . ronly
                          when inschool $ do
                              s0 <- pvQSearch a b 0
                              lift $ learn d typ s s0
                          when (d >= minToStore) $
                              -- store as upper score - move does not matter
                              lift $ store d typ s (head edges) deltan	-- (nodes1 - nodes0)
                      -- else modify $ \st -> st { killer = pushKiller (head p) s kill }
                      else modify $ \st -> st { killer = kill1 }
                  return (s, p)

{-# SPECIALIZE nullEdgeFailsHigh :: Node m e Int => Int -> Int -> Int -> Int -> Search m e Int Bool #-}
nullEdgeFailsHigh :: Node m e s => s -> s -> Int -> Int -> Search m e s Bool
nullEdgeFailsHigh a b d lastnull =
    if not nulActivate || lastnull < 1
       then return False
       else do
         tact <- lift tactical
         if tact
            then return False
            else do
               lift nullEdge	-- do null move
               inschool <- gets $ school . ronly
               let !nmb = if nulSubAct && not inschool then b - nulSubmrg else b
                   !a1  = -nmb
                   !b1  = a1 + nulMargin
                   !d1  = d - 1 - nulRedux
                   !lastnull1 = lastnull - 1
               val <- liftM fst $ pvSearch a1 b1 d1 [] lastnull1
               lift nullEdge	-- undo null move
               return $! (-val) >= nmb

-- This is the inner loop of the PV search, executed at every level (except root) once per possible move
-- See the parameter
-- Returns: flag if it was a beta cut and new status
{-# SPECIALIZE pvInnerLoop :: Node m e Int => Int -> Int -> [e] -> e -> Search m e Int (Bool, [e]) #-}
pvInnerLoop :: Node m e s
            => s	-- current beta
            -> Int	-- current search depth
            -> [e]	-- "status" is here the pv continuation
            -> e	-- move to search
            -> Search m e s (Bool, [e])
pvInnerLoop b d lastpath e = do
    old <- get
    let !a = cursc old
    exd <- lift $ doEdge e False	-- do the move
    newNode
    modify $ \s -> s { absdp = absdp s + 1 }
    (s, p) <- case exd of
                Exten exd' -> pvInnerLoopExten b d (special e) exd' lastpath
                Final sco  -> return (sco, [])
    lift $ undoEdge e	-- undo the move
    modify $ \s -> s { absdp = absdp s - 1 }
    checkFailOrPVLoop old a b d e s p

{-# SPECIALIZE pvInnerLoopExten :: Node m e Int => Int -> Int -> Bool -> Int -> [e] -> Search m e Int (Int, [e]) #-}
pvInnerLoopExten :: Node m e s => s -> Int -> Bool -> Int -> [e] -> Search m e s (s, [e])
pvInnerLoopExten b d spec exd lastpath = do
    old <- get
    tact <- lift tactical
    exd' <- if usedext old >= maxDepthExt
              then return 0
              else do
                modify $ \s -> s { usedext = usedext s + exd }
                return exd
    let !mn = movno old
        !a = cursc old
        -- late move reduction
        !reduce = not (nearmate a || tact || spec || exd > 0 || d < lmrMinDRed)
        !(d', reduced) = nextDepth (d+exd') (draft old) mn reduce (forpv old)
        !a1 = -b
        !b1 = -a
    if forpv old 	-- && a < b - 1	-- to get really PV (otherwise cut/all)
       then do
          pvpath <- if null lastpath
                      then bestMoveFromHash
                      else return lastpath
          pvSearch a1 b1 d' pvpath nulMoves
       else do
          (hdeep, tp, hscore, e', nodes)
              <- if d >= minToRetr
                    then do
                        reTrieve
                        lift retrieve
                    else return (-1, 0, 0, undefined, 0)
          -- let pvpath = if null lastpath
          --           then if hdeep > 0 && tp > 0 then [e'] else []
          --           else lastpath
          let !pvpath = if hdeep > 0 && tp > 0 then [e'] else lastpath
              !hs = - hscore
          if hdeep >= d && (tp == 2 || tp == 1 && hs > a || tp == 0 && hs <= a)
             then reSucc nodes >> return (hscore, [e'])
             else do
                 -- futility pruning
                 inschool <- gets $ school . ronly
                 (prune, v) <- if tact || inschool	-- don't prune when tactical or in learning
                                  then return (False, 0)
                                  else isPruneFutil d (-b) (-a)
                 if prune
                    then return (v, [])	-- we will fail low or high
                    else do
                       nulWind
                       let !a2 = b1 - 1
                       r@(s1, _) <- pvSearch a2 b1 d' pvpath nulMoves
                       if (-s1) > a		-- we need re-search
                          then do
                            reSearch
                            if reduced && d >= lmrMinDFRes
                               then do	-- re-search with no reduce is expensive!
                                  let !d'' = fst $ nextDepth (d+exd') (draft old) mn False (forpv old)
                                  pvSearch a1 b1 d'' pvpath nulMoves
                               else pvSearch a1 b1 d' pvpath nulMoves
                          else return r

{-# SPECIALIZE checkFailOrPVLoop :: Node m e Int => PVState e Int -> Int -> Int -> Int -> e -> Int -> [e] -> Search m e Int (Bool, [e]) #-}
checkFailOrPVLoop :: Node m e s => PVState e s -> s -> s -> Int -> e -> s -> [e] -> Search m e s (Bool, [e])
checkFailOrPVLoop old a b d e s' p = do
    xstats <- gets stats
    let !mn = movno old
        !np = e : p
        !s = - s'
        !nodes0 = sNodes $ stats old
        !nodes1 = sNodes xstats
        !nodes  = nodes1 - nodes0
    inschool <- gets $ school . ronly
    if s >= b
       then do
            let typ = 1	-- best move is e and is beta cut (score is lower limit)
            when (d >= minToStore) $ lift $ store d typ s e nodes
            lift $ betaMove True d (absdp old) e -- anounce a beta move (for example, update history)
            when inschool $ do
                s0 <- pvQSearch a b 0
                lift $ learn d typ b s0
            -- when debug $ logmes $ "<-- pvInner: beta cut: " ++ show s ++ ", return " ++ show b
            put old { cursc = nextlev b, path = np, weak = False, stats = statCut xstats mn }
            return (True, [])
       else if s > a
          then do
              let typ = 2	-- score is exact
              when (forpv old || d >= minToStore) $ lift $ store d typ s e nodes
              -- when debug $ logmes $ "<-- pvInner - new a: " ++ show s
              when inschool $ do
                  s0 <- pvQSearch a b 0
                  lift $ learn d typ s s0
              let !mn1 = mn + 1
              put old { cursc = nextlev s, path = np, forpv = False,
                        movno = mn1, weak = False, stats = xstats }
              return (False, [])
          else do
              -- when in a cut node and the move dissapointed - negative history
              when (useNegHist && forpv old && a == b - 1 && mn <= negHistMNo)
                   $ lift $ betaMove False d (absdp old) e
              let !mn1 = mn + 1
              put old { movno = mn1, stats = xstats }
              return (False, [])

-- We don't sort the moves here, they have to come sorted from genEdges
-- But we consider the best moves first (best from previous iteration, killers)
{-# INLINE genAndSort #-}
genAndSort :: Node m e s => [e] -> Killer e s -> Int -> Bool -> Search m e s [e]
genAndSort lastpath kill d pv = do
    kl <- lift (filterM legalEdge $ killerToList kill)
    adp <- gets absdp 
    lift $ liftM (bestFirst lastpath kl) (genEdges d adp pv')
    where pv' = pv || not (null lastpath)

-- Late Move Reduction
{-# INLINE nextDepth #-}
nextDepth :: Int -> Int -> Int -> Bool -> Bool -> (Int, Bool)
nextDepth d rd w lmr pv = (max 0 nd, reduced)
    where nd = if lmr then d - k else d - 1
          idx = (min lmrMaxDepth d, min lmrMaxWidth w)
          k  = if pv then lmrReducePv ! idx else lmrReduceArr ! idx
          -- dg = (d + deGran - 1) `div` deGran
          reduced = nd < d - 1

-- This is a kind of monadic fold optimized for (beta) cut
{-# INLINE pvLoop #-}
pvLoop :: Monad m => (s -> e -> m (Bool, s)) -> s -> [e] -> m s
pvLoop _ s [] = return s
pvLoop f s (e:es) = do
    (cut, s') <- f s e
    if cut then return s'
           else pvLoop f s' es

{-# SPECIALIZE isPruneFutil :: Node m e Int => Int -> Int -> Int -> Search m e Int (Bool, Int) #-}
isPruneFutil :: Node m e s => Int -> s -> s -> Search m e s (Bool, s)
isPruneFutil d a b
    | d > maxFutilDepth = return (False, 0)
    | otherwise = do
        let !margin = futilMargins ! d
        -- v <- lift materVal	-- can we do here direct static evaluation?
        v <- pvQSearch a b 0
        if v < a && v + margin <= a
           then return (True, a)
           else if v > b && v - margin >= b
                then return (True, b)
                else return (False, 0)

-- PV Quiescent Search
{-# SPECIALIZE pvQSearch :: Node m e Int => Int -> Int -> Int -> Search m e Int Int #-}
pvQSearch :: Node m e s => s -> s -> Int -> Search m e s s
-- pvQSearch a b c | c > qsMaxChess = lift staticVal  -- limit the no of chess in qs path
-- pvQSearch a b c | c > qsMaxChess = return 0  -- TEST: consider here endless loop
pvQSearch a b c = do				   -- to avoid endless loops
    stp <- lift staticVal				-- until we can recognize repetition
    tact <- lift tactical
    if tact
       then do
           edges <- lift $ genEdges 0 0 False
           if null edges
              then return stp
              else if c >= qsMaxChess
                      then return inEndlessCheck
                      else do
                          -- for check extensions in case of very few moves (1 or 2):
                          -- if 1 move: search even deeper
                          -- if 2 moves: same depth
                          -- if 3 or more: no extension
                          let !esc = length $ take 3 edges
                              !nc = c + esc - 2
                              !a' = if stp > a then stp else a
                          pvLoop (pvQInnerLoop b nc) a' edges
       else if qsBetaCut && stp >= b
               then return b
               else do
                   let !delta = a - qsDelta
                   if qsDeltaCut && delta < a && stp < delta
                      then return a
                      else do
                          edges <- lift genTactEdges
                          if null edges
                             then return stp
                             else do
                                 let !a' = if stp > a then stp else a
                                 pvLoop (pvQInnerLoop b c) a' edges

-- {-# INLINE pvQInnerLoop #-}
{-# SPECIALIZE pvQInnerLoop :: Node m e Int => Int -> Int -> Int -> e -> Search m e Int (Bool, Int) #-}
pvQInnerLoop :: Node m e s => s -> Int -> s -> e -> Search m e s (Bool, s)
pvQInnerLoop b c a e = do
    -- here: delta pruning: captured piece + 200 > a? then go on, else return
    r <- lift $ doEdge e True
    newNodeQS
    -- s <- liftM negate $ case r of
    s <- liftM (nextlev . negate) $ case r of
            Final s -> return s
            _       -> pvQSearch (-b) (-a) c
    lift $ undoEdge e
    if s >= b
       then return (True, b)
       else if s > a
               then return (False, s)
               else return (False, a)

{-# SPECIALIZE bestMoveFromHash :: Node m e Int => Search m e Int [e] #-}
bestMoveFromHash :: Node m e s => Search m e s [e]
bestMoveFromHash = do
    reTrieve
    (hdeep, tp, _, e, _) <- lift retrieve
    when (hdeep > 0) $ reSucc 1		-- here we save just move generation
    -- return $! if hdeep > 0 && tp > 0 then [e] else []
    return $! [ e | hdeep > 0 && tp > 0 ]

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

bestFirst :: Eq e => [e] -> [e] -> [e] -> [e]
bestFirst path kl es
    | null path = kl ++ delall es kl
    | otherwise = e : kle ++ delall es (e : kl)
    where kle = delete e kl
          delall = foldr delete
          e = head path

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

killerToList :: (Show e, Show s) => Killer e s -> [e]
killerToList NoKiller = []
killerToList (OneKiller e _) = [e]
killerToList (TwoKillers e1 _ e2 _) = [e1, e2]

--- Communication to the outside - some convenience functions ---

informBM a b c d = inform (BestMv a b c d)

informCM a b = inform (CurrMv a b)

logmes s = inform (LogMes s)

informBest :: Node m e s => s -> Int -> [e] -> Search m e s ()
informBest s d es = do
    n <- lift curNodes
    lift $ informBM s d n es
