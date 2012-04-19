{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, BangPatterns #-}
module Uci.UciGlue (
    bestMoveCont
) where

import Data.Array.IArray
import Control.Monad.State.Lazy

import qualified Search.SearchMonad as SM
import Search.AlbetaTypes
import Search.Albeta
import Struct.Struct
import Struct.Status
import Struct.Context
import Moves.Base
import Eval.Eval

instance CtxMon CtxIO where
    tellCtx = talkToContext
    timeCtx = lift currMilli

-- Parameter of the search at this level:
aspirWindow :: Int
aspirWindow   = 24	-- initial aspiration window

showEvalStats :: Bool
showEvalStats = False	-- show eval statistics in logfile

-- One iteration in the search for the best move
bestMoveCont :: Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> CtxIO IterResult
bestMoveCont tiefe sttime stati lastsc lpv rmvs = do
    -- ctx <- ask
    informGuiDepth tiefe
    ctxLog "Info" $ "start search for depth " ++ show tiefe
    let abc = ABC {
                maxdepth = tiefe,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                learnev   = learnEval,
                best      = False,
                stoptime  = sttime
                }
    ((sc, path, rmvsf), statf) <- SM.runSearch (alphaBeta abc) stati
    when (sc == 0) $ return ()
    let n = nodes . stats $ statf
    informGui sc tiefe n path
    ctxLog "Info" $ "score " ++ show sc ++ " path " ++ show path
    -- let math = stats statf
    -- ctxLog "Info" $ "Node reads " ++ show (nreads math) ++ ", read hits "
    --     ++ show (rhits math) ++ ", read colls " ++ show (rcoll math)
    --     ++ ", node writes " ++ show (nwrites math) ++ ", write colls "
    --     ++ show (wcoll math)
    if learnEval && esSamples (evalst statf) > 0
        then do
            let evst = evalst statf
                evstf = evalState adjEval evst
            ctxLog "Info" $! "Eval params dev  : " ++ show (esDeviation evst)
                ++ " samples: " ++ show (esSamples evst)
            ctxLog "Info" $! "Eval params old D: " ++ show (esDParams evst)
            ctxLog "Info" $! "Eval params new D: " ++ show (esDParams evstf)
            ctxLog "Info" $! "Gradient step    : " ++ show (esAlpha evstf)
            ctxLog "Info" $! "Mean error amt   : " ++ show (esAmpl evstf)
            ctxLog "Info" $! "Last error angle : " ++ show (esAngle evstf)
            when (esIParams evst /= esIParams evstf) $ do
                ctxLog "Info" $! "Eval params I old / new (only changes):"
                forM_ (zip paramNames $ zip (esIParams evst) (esIParams evstf)) $
                    \(n', (vo, vn))
                        -> when (vn /= vo) $
                                ctxLog "Info" $! n' ++ "\t" ++ show vo ++ "\t" ++ show vn
            when (showEvalStats && tiefe >= 6) $
                ctxLog "Info" $! "Eval statistics: " ++ "\n"
                   ++ unlines [ "dpt " ++ show d ++ ": " ++ showline li
                                 | d <- [1..maxStatsDepth], let li = getline (esStats evst) d ]
            return (path, sc, rmvsf, statf { evalst = evstf } )
        else
            return (path, sc, rmvsf, statf)
    where showline = unwords . map show
          getline a l = [ a!(l, i) | i <- [0..maxStatsIntvs]]

talkToContext :: Comm -> CtxIO ()
talkToContext (LogMes s)       = ctxLog "Info" s
talkToContext (BestMv a b c d) = informGui a b c d
talkToContext (CurrMv a b)     = informGuiCM a b
talkToContext (InfoStr s)      = informGuiString s
