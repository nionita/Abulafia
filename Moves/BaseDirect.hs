{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             BangPatterns,
             RankNTypes, UndecidableInstances
             #-}

module Moves.BaseDirect (
    CtxMon(..),
    posToState, initPos, getPos,
    doMove, undoMove, genMoves, genTactMoves,
    useHash, learnEval,
    staticVal0, mateScore,
    showMyPos,
    nextlev, nearmate, toInt, fromInt, special
) where

import Data.Array.IArray
import Data.Array.Unboxed
import Debug.Trace
import Control.Exception (assert)
import Data.Word
import Data.Bits
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State.Lazy
import Control.Monad.Reader
import Data.Ord (comparing)
import Data.Array.IO
import System.Random

import Moves.BaseTypesD
import Search.AlbetaTypesD
import qualified Search.SearchMonadCPS as SM
-- import Search.Albeta
import Struct.Struct
import Hash.SimpleCache
import Struct.Status
import Moves.Board
import Moves.BitBoard
import Moves.SEE
import Eval.Eval
import Moves.ShowMe
import Moves.History

nextlev !i
    | i >= mateScore - 255    = -im
    | i <= (-mateScore) + 255 = -ip
    | otherwise               = -i
    where !ip = i + 1
          !im = i - 1
nearmate i = i >= mateScore - 255 || i <= -mateScore + 255
toInt   = id
fromInt = id

-- instance Edge Move where
{-# INLINE special #-}
special = moveIsSpecial

instance CtxMon m => Node (Game r m) where
    staticVal = staticVal0
    materVal  = materVal0
    genEdges = genMoves
    genTactEdges = genTactMoves
    {-# INLINE tactical #-}
    tactical = tacticalPos
    legalEdge = isMoveLegal
    killCandEdge = isKillCand
    inSeq  = okInSequence
    doEdge = doMove False
    undoEdge = undoMove
    betaMove = betaMove0
    nullEdge = doNullMove
    retrieve = currDSP
    store = storeSearch
    learn = learnEvals
    {-# INLINE curNodes #-}
    curNodes = getNodes
    inform = SM.lift . tellCtx
    choose  = choose0

-- Some options and parameters:
debug       = False
useHash     = False
learnEval   = False
depthForMovesSortPv = 1	-- use history for sorting moves when pv or cut nodes
depthForMovesSort   = 1	-- use history for sorting moves
-- scoreDiffEqual      = 4 -- under this score difference moves are considered to be equal (choose random)
scoreDiffEqual      = 0 -- under this score difference moves are considered to be equal (choose random)
minLearnDepth = 1
maxLearnDepth = 1

mateScore = 20000

getNodes :: CtxMon m => Game r m Int
{-# INLINE getNodes #-}
getNodes = gets (nodes . stats)

{-# INLINE getPos #-}
getPos :: CtxMon m => Game r m MyPos
getPos = gets (head . stack)

posToState :: MyPos -> Cache -> History -> EvalState -> MyState
posToState p c h e = MyState {
                       stack = [updatePos p],
                       hash = c,
                       hist = h,
                       stats = stats0,
                       evalst = e
                   }
    where stats0 = Stats {
                       nodes = 0,
                       maxmvs = 0
                   }

debugGen = False

useCaptWL = False
loosingLast = False

genMoves :: CtxMon m => Int -> Int -> Bool -> Game r m ([Move], [Move])
genMoves depth absdp pv = do
    p <- getPos
    -- when debugGen $ do
    --     lift $ ctxLog "Debug" $ "--> genMoves:\n" ++ showTab (black p) (slide p) (kkrq p) (diag p)
    let !c = moving p
        lc = map (genmv True p) $ genMoveFCheck p c
    if isCheck p c
       then return (lc, [])
       else do
            let l0 = genMoveCast p c
                l1 = map (genmvT p) $ genMoveTransf p c
                l2 = map (genmv True p) $ genMoveCapt p c
                (pl2w, pl2l) = genMoveCaptWL p c
                l2w = map (genmv True p) pl2w
                l2l = map (genmv True p) pl2l
                l3'= map (genmv False p) $ genMoveNCapt p c
            l3 <- if pv && depth >= depthForMovesSortPv
                     || not pv && depth >= depthForMovesSort
                     -- then sortMovesFromHash l3'
                     then sortMovesFromHist absdp l3'
                     else return l3'
            -- return $! if useCaptWL
            --             then if loosingLast
            --                     then concat [l1, l2w, l0, l3, l2l]
            --                     else concat [l1, l2w, l2l, l0, l3]
            --             else (l1 ++ l2 ++ l0, l3)
            return (l1 ++ l2, l0 ++ l3)

onlyWinningCapts = True

-- Generate only tactical moves, i.e. promotions, captures & check escapes
genTactMoves :: CtxMon m => Game r m [Move]
genTactMoves = do
    p <- getPos
    let !c = moving p
        l1 = map (genmvT p) $ genMoveTransf p c
        l2 = map (genmv True p) $ genMoveCapt p c
        lnc = map (genmv True p) $ genMoveNCaptToCheck p c
        (pl2, _) = genMoveCaptWL p c
        l2w = map (genmv True p) pl2
        lc = map (genmv True p) $ genMoveFCheck p c
        -- the non capturing check moves have to be at the end (tested!)
        -- else if onlyWinningCapts then l1 ++ l2w ++ lnc else l1 ++ l2 ++ lnc
        !mvs | isCheck p c      = lc
             | onlyWinningCapts = l1 ++ l2w
             | otherwise        = l1 ++ l2
    return mvs

sortMovesFromHist :: CtxMon m => Int -> [Move] -> Game r m [Move]
sortMovesFromHist d mvs = do
    s <- get
    -- mvsc <- liftIO $ mapM (\m -> fmap negate $ valHist (hist s) (fromSquare m) (toSquare m) d) mvs
    mvsc <- liftIO $ mapM (\m -> valHist (hist s) (fromSquare m) (toSquare m) d) mvs
    -- return $ map fst $ sortBy (comparing snd) $ zip mvs mvsc
    let (posi, zero) = partition ((/=0) . snd) $ zip mvs mvsc
    return $! map fst $ sortBy (comparing snd) posi ++ zero

massert :: CtxMon m => String -> Game r m Bool -> Game r m ()
massert s mb = do
    b <- mb
    if b then return () else error s

{-# INLINE statNodes #-}
statNodes :: CtxMon m => Game r m ()
statNodes = do
    s <- get
    let st = stats s
        !n = nodes st + 1
        !s1 = s { stats = st { nodes = n } }
    put s1

showMyPos :: MyPos -> String
showMyPos p = showTab (black p) (slide p) (kkrq p) (diag p) ++ "================\n"

-- move from a node to a descendent
doMove :: CtxMon m => Bool -> Move -> Bool -> Game r m DoResult
doMove real m qs = do
    statNodes   -- when counting all visited nodes
    s  <- get
    -- let pc = if null (stack s) then error "doMove" else head $ stack s
    let !pc = head $ stack s
        !m1 = if real then checkCastle (checkEnPas m pc) pc else m
        !kc = case tabla pc (toSquare m1) of
                  Busy _ King -> True
                  _           -> False
        !p' = doFromToMove m1 pc { realMove = real }
        !c = moving p'
        !kingcapt = illegalPos p' || kc
        (!sts, feats) = if real then (0, []) else evalState (posEval p' c) (evalst s)
        !p = p' { staticScore = sts, staticFeats = feats }
        !dext = if inCheck p || goPromo p m1 then 1 else 0
    -- when debug $
    --     lift $ ctxLog "Debug" $ "*** doMove: " ++ showMyPos p
    -- remis' <- checkRepeatPv p pv
    -- remis  <- if remis' then return True else checkRemisRules p
    put s { stack = p : stack s }
    remis <- if qs then return False else checkRemisRules p'
    if kingcapt
       then return $ Final mateScore
       else if remis
               then return $ Final 0
               else -- do
                    -- case p of _ -> return ()
                    return $ Exten dext

doNullMove :: CtxMon m => Game r m ()
doNullMove = do
    s <- get
    let !p0 = if null (stack s) then error "doNullMove" else head $ stack s
        !ps = tail $ stack s
        !p' = reverseMoving p0
        !c = moving p'
        (!sts, feats) = evalState (posEval p' c) (evalst s)
        !p = p' { staticScore = sts, staticFeats = feats }
    put s { stack = p : ps }

checkRemisRules :: CtxMon m => MyPos -> Game r m Bool
checkRemisRules p = do
    s <- get
    if remis50Moves p
       then return True
       else do	-- check repetition rule
         let revers = map zobkey $ takeWhile isReversible $ stack s
             equal  = filter (== zobkey p) revers	-- if keys are equal, pos is equal
         case equal of
            (_:_:_)    -> return True
            _          -> return False

checkRepeatPv :: CtxMon m => MyPos -> Bool -> Game r m Bool
checkRepeatPv _ False = return False
checkRepeatPv p _ = do
    s <- get
    let search = map zobkey $ takeWhile imagRevers $ stack s
        equal  = filter (== zobkey p) search	-- if keys are equal, pos is equal
    case equal of
        (_:_) -> return True
        _     -> return False
    where imagRevers t = isReversible t && not (realMove t)

{-# INLINE undoMove #-}
undoMove :: CtxMon m => Move -> Game r m ()
undoMove m = modify $ \s -> s { stack = tail $ stack s }

-- Tactical positions will be searched complete in quiescent search
-- Currently only when in in check
{-# INLINE tacticalPos #-}
tacticalPos :: CtxMon m => Game r m Bool
tacticalPos = do
    t <- getPos
    return $! check t /= 0

{-# INLINE isMoveLegal #-}
isMoveLegal :: CtxMon m => Move -> Game r m Bool
isMoveLegal m = do
    t <- getPos
    return $! legalMove t m

isKillCand :: CtxMon m => Move -> Move -> Game r m Bool
isKillCand mm ym
    | toSquare mm == toSquare ym = return False
    | otherwise = do
        t <- getPos
        return $! nonCapt t ym

okInSequence :: CtxMon m => Move -> Move -> Game r m Bool
okInSequence m1 m2 = do
    t <- getPos
    return $! alternateMoves t m1 m2

-- Static evaluation function
{-# INLINE staticVal0 #-}
staticVal0 :: CtxMon m => Game r m Int
staticVal0 = do
    s <- get
    t <- getPos
    let !c = moving t
        !stSc = if illegalPos t
                   then error $ "Wrong position, pos stack:\n" ++ concatMap showMyPos (stack s)
                   else staticScore t
        -- Here we actually don't need genMoves - it would be enough to know that
        -- there is at leas one legal move, which should be much cheaper
        stSc1 | hasMoves t c  = stSc
              | check t /= 0  = -mateScore
              | otherwise     = 0
    -- when debug $ lift $ ctxLog "Debug" $ "--> staticVal0 " ++ show stSc1
    return $! stSc1

materVal0 :: CtxMon m => Game r m Int
materVal0 = do
    t <- getPos
    let !m = mater t
    return $! case moving t of
                   White -> m
                   _     -> -m

quiet :: MyPos -> Bool
quiet p = at .&. ta == 0
    where (!at, !ta) = if moving p == White then (whAttacs p, black p) else (blAttacs p, white p)

learnEvals :: CtxMon m => Int -> Int -> Int -> Int -> Game r m ()
learnEvals depth typ score score0 = do
    s <- get
    t <- getPos
    let sts = staticScore t
        -- fscore = fLearnMatEnPri (staticFeats t)
    -- if not learnEval || not (quiet t) || null (staticFeats t)
    --   || typ == 1 && sts >= score || typ == 0 && sts <= score || typ == 2 && sts == score
    if not learnEval || null (staticFeats t)
       -- || typ == 1 && sts >= score || typ == 0 && sts <= score
       || typ == 1 && score0 >= score || typ == 0 && score0 <= score
       || depth < minLearnDepth || depth > maxLearnDepth
       then return ()
       else do
           -- let nevst = evalState (cntEval depth sts score (staticFeats t)) (evalst s)
           let nevst = evalState (cntEval depth score0 score (staticFeats t)) (evalst s)
           -- Test: try to learn other functions of the features:
           -- let nevst = evalState (cntEval depth sts fscore (staticFeats t)) (evalst s)
           put s { evalst = nevst }

-- Test: learn other (teoretical) functions (of position features)
-- Here: sum of material and en prise fraction
-- fLearnMatEnPri :: [Int] -> Int
-- fLearnMatEnPri xs = xs!!0 + xs!!1	-- shoud drive the parameters to [1, 1, 0, ...]

{-# INLINE currDSP #-}
currDSP :: CtxMon m => Game r m (Int, Int, Int, Move, Int)
currDSP = if not useHash then return empRez else do
    -- when debug $ lift $ ctxLog "Debug" $ "--> currDSP "
    s <- get
    p <- getPos
    -- mhr <- liftIO $ readCache (hash s) (basicPos p)
    mhr <- liftIO $ readCache (hash s) p
    -- let (r, sc) = case mhr of
               -- Just t@(_, _, sco, _, _) -> (t, sco)
               -- _      -> (empRez, 0)
    let r = case mhr of
               Just t -> t
               _      -> empRez
    -- when debug $ lift $ ctxLog "Debug" $ "*** currDSP " ++ show r ++ " zkey " ++ show (zobkey p)
    -- when (sc `mod` 4 /= 0) $ liftIO $ putStrLn $ "info string In currDSP: " ++ show r
    return r
    where empRez = (-1, 0, 0, Move 0, 0)

{-# INLINE storeSearch #-}
storeSearch :: CtxMon m => Int -> Int -> Int -> Move -> Int -> Game r m ()
storeSearch deep tp sc best nodes = if not useHash then return () else do
    s <- get
    p <- getPos
    -- when (sc `mod` 4 /= 0 && tp == 2) $ liftIO $ do
    --     putStrLn $ "info string In storeSearch: tp = " ++ show tp ++ " sc = " ++ show sc
    --         ++ " best = " ++ show best ++ " nodes = " ++ show nodes
        -- putStrLn $ "info string score in position: " ++ show (staticScore p)
    -- We use the type: 0 - upper limit, 1 - lower limit, 2 - exact score
    st <- liftIO $ writeCache (hash s) p sc tp deep best nodes
    -- when debug $ lift $ ctxLog "Debug" $ "*** storeSearch (deep/tp/sc/mv) " ++ show deep
    --      ++ " / " ++ show tp ++ " / " ++ show sc ++ " / " ++ show best
    --      ++ " status: " ++ show st ++ " (" ++ show (zobkey p) ++ ")"
    return ()

-- History heuristic table update when beta cut move
betaMove0 :: CtxMon m => Bool -> Int -> Int -> Move -> Game r m ()
betaMove0 good dp absdp m = do
    s <- get
    t <- getPos
    -- liftIO $ toHist (hist s) good (fromSquare m) (toSquare m) absdp
    case tabla t (toSquare m) of
        Empty -> liftIO $ toHist (hist s) good (fromSquare m) (toSquare m) absdp
        _     -> return ()

{--
showChoose :: CtxMon m => [] -> Game m ()
showChoose pvs = do
    mapM_ (\(i, (s, pv)) -> SM.lift $ ctxLog "Info"
                                 $ "choose pv " ++ show i ++ " score " ++ show s ++ ": " ++ show pv)
                 $ zip [1..] pvs
    return $ if null pvs then error "showChoose" else head pvs
--}

-- Choose between almost equal (root) moves
choose0 :: CtxMon m => Bool -> [(Int, [Move])] -> Game r m (Int, [Move])
choose0 True pvs = return $ head pvs
choose0 _    pvs = case pvs of
    p1 : [] -> return p1
    p1 : ps -> do
         let equal = p1 : takeWhile inrange ps
             len = length equal
             inrange x = fst p1 - fst x <= scoreDiffEqual
         logMes $ "Choose length: " ++ show len
         if len == 1
            then return p1
            else do
               r <- liftIO $ getStdRandom (randomR (0, len - 1))
               return $! equal !! r

logMes :: CtxMon m => String -> Game r m ()
logMes s = SM.lift $ tellCtx . LogMes $ s
