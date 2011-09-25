module Main (main) where

import Control.Monad
import Control.Monad.State (evalState)
-- import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
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
import Hash.SimpleCache
import Search.AlbetaTypes
import Search.Albeta
import Search.SearchMonad
import Config.ConfigClass
import Config.Config
-- import Moves.Notation

-- Here we work in the old plain IO monad:
instance CtxMon IO where
    tellCtx = tellInIO

main = do
    args <- getArgs
    if length args < 2
       then putStrLn "Start it with depth and fen!"
       else do
           let depth = read $ head args
               fen   = unwords $ tail args
               pos = updatePos $ posFromFen fen
           putStrLn $ "Analise depth " ++ show depth ++ " fen " ++ fen
           ha  <- newCache defaultConfig
           hi  <- newHist
           evs <- makeEvalState Nothing
           let inist = posToState pos ha hi evs
           searchTheTree 1 depth inist Nothing [] []

tellInIO :: Comm Move Int -> IO ()
tellInIO (LogMes s) = putStrLn $ "Log: " ++ s
tellInIO (BestMv a b c d) = putStrLn $ "info score " ++ show a ++ " depth " ++ show b
                                         ++ " nodes " ++ show c ++ " pv " ++ show d
tellInIO (CurrMv a b) = putStrLn $ "info currmove " ++ show a ++ " currmovenumber " ++ show b
tellInIO (InfoStr s) = putStrLn s
tellInIO _ = return ()

-- Parameter of the search at this level:
aspirWindow   = 16	-- initial aspiration window
showEvalStats = False	-- show eval statistics in logfile

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
                learnev   = False,
                best      = True
                }
    ((sc, path, rmvsf), statf) <- runSearch (alphaBeta abc) stati
    when (sc == 0) $ return ()
    let n = nodes . stats $ statf
    tellInIO (BestMv sc tiefe n path)
    -- informGui sc tiefe n path
    -- ctxLog "Info" $ "score " ++ show sc ++ " path " ++ show path
    -- let math = stats statf
    -- ctxLog "Info" $ "Node reads " ++ show (nreads math) ++ ", read hits "
    --     ++ show (rhits math) ++ ", read colls " ++ show (rcoll math)
    --     ++ ", node writes " ++ show (nwrites math) ++ ", write colls "
    --     ++ show (wcoll math)
    return (path, sc, rmvsf, statf)

searchTheTree :: Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> IO ()
searchTheTree tief mtief mystate lsc lpv rmvs = do
    -- search with the given depth
    (path, sc, rmvsf, stfin) <- bestMoveCont tief mystate lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness!
    if tief >= mtief  -- maximal depth
        then giveBestMove path (nodes $ stats stfin)
        else searchTheTree (tief + 1) mtief stfin (Just sc) path rmvsf

giveBestMove :: [Move] -> Int -> IO ()
giveBestMove mvs nodes = putStrLn $ " -> bestmove: " ++ bm ++ ", nodes: " ++ show nodes
    where bm = sead . words . show . head $ mvs
          sead = head . tail

isLearn = False

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState argfile =
    case argfile of
        Just afn -> do
            fex <- doesFileExist afn
            if fex then filState afn else defState
        Nothing -> defState
    where defState = return $ initEvalState isLearn []
          filState fn = fmap (initEvalState isLearn) (fileToParams `fmap` readFile fn)

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
