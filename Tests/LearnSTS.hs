module Main (main) where

import Control.Monad
import Control.Monad.State (evalState)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Vector.Unboxed (toList)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
-- import System.Time

import Struct.Struct
import Struct.Status
import Moves.Moves
import Moves.Board
import Moves.Base
import Moves.History
import Eval.Eval
import Eval.OptParams
import Hash.SimpleCache
import Search.Albeta
import Search.SearchMonad
import Config.ConfigClass
import Config.Config

-- Here we work in the old plain IO monad:
instance CtxMon IO where
    tellCtx = tellInIO

searchDepth = 4

main = do
    args <- getArgs
    let posfn = args!!0
        alen  = length args
        lin   = if alen > 1 then Just $ read $ args!!1 else Nothing
        (def, dist0) = initPars
    putStrLn $ "Reading positions from " ++ posfn ++ "..."
    poss <- B.readFile (head args) >>= return . map perPos . mtake lin . B.lines
    putStrLn $ show (length poss) ++ " positions read"
    putStrLn "Optimizing..."
    best <- optimize (fitness poss) (inLimits parLims) def dist0
    putStrLn "======================"
    putStrLn "Optimum is:"
    forM_ (zip paramNames best) $ \(n, v) -> putStrLn $ n ++ " = " ++ show v
    where mtake Nothing  = id
          mtake (Just n) = take n

perPos :: B.ByteString -> MyPos
perPos fen = updatePos True $ posFromFen $ B.unpack fen

tellInIO :: Comm Move Int -> IO ()
-- tellInIO (LogMes s) = putStrLn $ "Info: " ++ s
tellInIO _ = return ()

fitness :: [MyPos] -> Vec -> IO Double
fitness poss p = mapM (poserr p) poss >>= return . sum

depth1 = 0
depth2 = 2

poserr :: Vec -> MyPos -> IO Double
poserr p pos = do
    ha  <- newCache defaultConfig
    hi  <- newHist
    let evs = initEvalStateV $ toList p
        inist = posToState pos ha hi evs
    sc1 <- searchTheTree 1 depth1 inist Nothing [] []
    sc2 <- searchTheTree 1 depth2 inist Nothing [] []
    let d = sc2 - sc1
    return $! fromIntegral $ d * d

-- Parameter of the search at this level:
aspirWindow   = 16	-- initial aspiration window

-- One iteration in the search for the best move
bestMoveCont :: Int -> MyState -> Maybe Int -> [Move] -> [Move] -> IO ([Move], Int, [Move], MyState)
bestMoveCont tiefe stati lastsc lpv rmvs = do
    -- informGuiDepth tiefe
    -- ctxLog "Info" $ "start search for depth " ++ show tiefe
    let abc = ABC {
                maxdepth = tiefe,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                learnev   = False
                }
    ((sc, path, rmvsf), statf) <- runSearch (alphaBeta abc) stati
    when (sc == 0) $ return ()
    let n = nodes . stats $ statf
    return (path, sc, rmvsf, statf)

searchTheTree :: Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> IO Int
searchTheTree tief mtief mystate lsc lpv rmvs = do
    -- search with the given depth
    (path, sc, rmvsf, stfin) <- bestMoveCont tief mystate lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness!
    if tief >= mtief  -- maximal depth
        then return sc
        else searchTheTree (tief + 1) mtief stfin (Just sc) path rmvsf

initPars = (def, (parDim, (def, vars0)))
    where ies = initEvalState False []
          def = esDParams ies
          vars0 = map f parLims
          f (mi, ma) = max (abs mi) (abs ma)

{--
giveBestMove :: [Move] -> Int -> IO ()
giveBestMove mvs nodes = putStrLn $ " -> bestmove: " ++ bm ++ ", nodes: " ++ show nodes
    where bm = sead . words . show . head $ mvs
          sead = head . tail

isLearn = False

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState argfile = do
    case argfile of
        Just afn -> do
            fex <- doesFileExist afn
            if fex then filState afn else defState
        Nothing -> defState
    where defState = return $ initEvalState isLearn []
          filState fn = fileToParams `fmap` readFile fn >>= return . initEvalState isLearn

fileToParams = map readParam . nocomments . lines
    where nocomments = filter (not . iscomment)
          iscomment [] = True
          iscomment ('-':'-':_) = True
          iscomment (c:cs) | isSpace c = iscomment cs
          iscomment _ = False

readParam :: String -> (String, Double)
readParam s = let (ns, vs) = span (/= '=') s in (strip ns, cleanread vs)
    where strip = filter (not . isSpace)
          cleanread = read . tail . strip
--}
