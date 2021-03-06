{-# LANGUAGE PatternGuards #-}
module Main.Selfplay where
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Array.Unboxed
import Data.Char (isSpace)
import Data.Maybe
import Data.List
import Data.Ord (comparing)
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import qualified Control.Exception as CE
import System.Directory
import System.Environment (getArgs)
import System.IO
import System.Time

import Struct.Struct
import Struct.Status
import Struct.Context2
import Config.ConfigClass
import Hash.TransTab
import Uci.UCI
import Uci.UciGlue
import Moves.Base
import Moves.Moves (movesInit)
import Moves.Board (posFromFen)
import Moves.History
import Search.SearchMonad (execSearch)
import Eval.Eval (paramNames)
import Eval.FileParams (makeEvalState, learnConfigFilePrefix)

forceLogging = True || learnEval

initContext :: GConfig -> String -> String -> [String] -> IO Context
initContext cf@(GConfig cfg) pf1 pf2 opts = do
    s0 <- currentSecs
    let llev = getIParamDefault cfg "logLevel" 0
        logg = forceLogging || llev > 0 
    mlchan <- if logg then Just `fmap` newChan else return Nothing
    wchan  <- newChan
    ha1 <- newCache cfg
    hi1 <- newHist
    (parc1, evs1) <- makeEvalState learnEval cfg progVersion (Just pf1)
    ha2 <- newCache cfg
    hi2 <- newHist
    (parc2, evs2) <- makeEvalState learnEval cfg progVersion (Just pf2)
    let gsts = decodeGameStats opts
        change = Chg {
            config = cf,
            compThread = Nothing,
            crtPos = initPos,
            crtId  = evs1
            crtStatus = posToState initPos ha1 hi1 evs1,
            crtStats  = gsts,
            nxtId  = evs2,
            nxtStatus = posToState initPos ha2 hi2 evs2,	-- here: pos is dummy
            nxtStats  = gsts,
            forGui = Nothing,
            srchStrtMs = 0,
            myColor = White
         }
    ctxVar <- newMVar change
    let context = Ctx {
            szero  = s0,
            logger = mlchan,
            writer = wchan,
            loglev = llev,
            evpid  = parc,
            change = ctxVar
         }
    return context

main :: GConfig -> IO ()
main = do
    args <- getArgs
    if length args < 2 then error "Need at least 2 config files as arguments" else do
    let [pf1:pf2:opts] = args
    e1 <- fileExists pf1
    e2 <- fileExists pf2
    if e1 && e2
       then do
           ctx <- initContext (GConfig defaultConfig) pf1 pf2 opts
           runReaderT startTheMachine ctx
       else do
           if not e1 then putStrLn $ "File " ++ pf1 ++ " does not exists"
           if not e2 then putStrLn $ "File " ++ pf2 ++ " does not exists"

startTheMachine :: CtxIO ()
startTheMachine = do
    ctx <- ask
    let logg = forceLogging || loglev ctx > 0 
    when logg $ do
        crtt <- liftIO currentSecs
        let logFileName = progLogName ++ show crtt ++ ".log"
        startLogger logFileName
    startWriter
    beforeReadLoop
    runTheGame
    -- whatever to do when ending:
    beforeProgExit

startLogger :: String -> CtxIO ()
startLogger file = do
    ctx <- ask
    lh <- liftIO $ openFile file AppendMode
    liftIO $ forkIO $ CE.catch (theLogger (fromJust $ logger ctx) lh) exc
    ctxLog "Info" "Logger started"
    return ()
    where exc :: CE.SomeException -> IO ()
          exc e = putStrLn $ "Fatal: logger exception: " ++ show e

startWriter :: CtxIO ()
startWriter = do
    ctx <- ask
    liftIO $ forkIO $ theWriter (writer ctx) (logger ctx)
    return ()

startInformer :: CtxIO ()
startInformer = do
    ctx <- ask
    newThread (theInformer (inform ctx))
    return ()

theLogger :: Chan String -> Handle -> IO ()
theLogger lchan h = do
    s <- readChan lchan
    hPutStrLn h s
    hFlush h
    theLogger lchan h

theWriter :: Chan String -> Maybe (Chan String) -> IO ()
theWriter wchan mlchan = do
    s <- readChan wchan
    hPutStrLn stdout s
    hFlush stdout
    logging mlchan "Output" s
    theWriter wchan mlchan

theInformer :: Chan InfoToGui -> CtxIO ()
theInformer ichan = do
    s <- liftIO $ readChan ichan
    chg <- readChanging
    when (working chg) $ toGui s
    -- ctxLog "Debug" "Informer: go take next info if any..."
    theInformer ichan

toGui s = case s of
            InfoS s    -> answer $ infos s
            InfoD _    -> answer $ formInfoDepth s
            InfoCM _ _ -> answer $ formInfoCM s
            _          -> answer $ formInfo s

theReader :: CtxIO ()
theReader = do
    line <- liftIO getLine
    let euci = parseUciStr line
    stop <- case euci of
        Left _    -> do
            ctxLog "Input" line
            ctxLog "Parse" $ show euci
            return False
        Right uci -> interpret uci
    unless stop theReader

runTheGame :: CtxIO ()
runTheGame = do
    chg <- readChanging
    -- here: must check if it's end of game: mate, 50 moves rule, 3 repetitions
    -- or other conditions (accepted draw, resign), in which case terminate
    

-- Todo: decode correct game parameters (time or nodes)
-- Currently: node limit, 400k+20k, which corresponds to
-- 10+0.5 seconds per game and engine
decodeGameStats :: [String] -> GameStats
decodeGameStats _ = GameStats {
                        gsUsedTime  = 0, gsRemTime  = 0,   gsMoveTime  = 0,
                        gsUsedNodes = 0, gsRemNodes = 400, gsMoveNodes = 20
                    }

doPosition :: Pos -> [Move] -> CtxIO ()
doPosition fen mvs = do
    -- ctxLog "Info" $ "Position: " ++ show fen ++ " moves " ++ show mvs
    chg <- readChanging
    if working chg
        then ctxLog "Warn" "GUI sent Position while I'm working..."
        else do
            hi <- liftIO newHist
            let es = evalst $ crtStatus chg
            ns <- newState fen mvs (hash . crtStatus $ chg) hi es
            -- ns <- newState fen mvs
            modifyChanging (\c -> c { crtStatus = ns, myColor = myCol })
    where newState fpos ms c h es = foldM execMove (stateFromFen fpos c h es) ms
          -- execMove p m = execStateT (doMove True m False) p
          execMove p m = execSearch (doMove True m False) p
          fenColor = movingColor fen
          myCol = if even (length mvs) then fenColor else other fenColor

stateFromFen StartPos  c h es = posToState initPos c h es
stateFromFen (Pos fen) c h es = posToState (posFromFen fen) c h es

movingColor fen
    | Pos str <- fen
        = case head . head . tail $ words str of
              'w' -> White
              _   -> Black
    | otherwise = White     -- startposition

doGo cmds = do
    ctxLog "Info" $ "Go: " ++ show cmds
    chg <- readChanging
    if working chg
        then ctxLog "Warn" "GUI sent Go while I'm working..."
        else if Ponder `elem` cmds
            then ctxLog "Info" "Just ponder: ignored"
            else do
                md <- getIParamDef "maxDepth" 20
                let (tim, tpm, mtg) = getTimeParams cmds lastsc $ myColor chg
                    dpt = fromMaybe md (findDepth cmds)
                    lastsc = case forGui chg of
                                 Just InfoB { infoScore = sc } -> sc
                                 _ -> 0
                startWorking tim tpm mtg dpt

getTimeParams cs lastsc c
    = if tpm == 0 && tim == 0
         then (0, 0, 0)
         else (tim, tpm, mtg)
    where tpm = fromMaybe 0 $ findTInc c cs
          tim = fromMaybe 0 $ findTime c cs
          mtg = fromMaybe 0 $ findMovesToGo cs

timeReserved   = 20	-- milliseconds reserved for move communication
remTimeFracIni = 0.01	-- fraction of remaining time which we can consume at once - initial value
remTimeFracFin = 0.5	-- same at final (when remaining time is near zero)
remTimeFracDev = remTimeFracFin - remTimeFracIni

compTime tim tpm fixmtg lastsc
    = if tpm == 0 && tim == 0 then (0, 0) else (ctm, tmx)
    where ctn = tpm + tim `div` mtg
          ctm = if tim > 0 && tim < 8000 || tim == 0 && tpm < 1500 then 200 else ctn
          mtg = if fixmtg > 0 then fixmtg else estimateMovesToGo lastsc
          frtim = fromIntegral $ max 0 $ tim - ctm	-- rest time after this move
          fctm  = fromIntegral ctm
          rtimprc = fctm / max frtim fctm
          rtimfrc = remTimeFracIni + remTimeFracDev * rtimprc
          tmxt = round $ fctm + rtimfrc * frtim
          tmx  = min (tim - timeReserved) tmxt

estMvsToGo :: Array Int Int
estMvsToGo = listArray (0, 8) [30, 28, 24, 18, 12, 10, 8, 6, 3]

estimateMovesToGo sc = estMvsToGo ! mvidx
    where mvidx = min 8 $ abs sc `div` 100

-- Some parameters (until we have a good solution)
clearHash = False

newThread :: CtxIO () -> CtxIO ThreadId
newThread a = do
    ctx <- ask
    liftIO $ forkIO $ runReaderT a ctx

startWorking :: Int -> Int -> Int -> Int -> CtxIO ()
startWorking tim tpm mtg dpt = do
    currms <- lift currMilli
    ctxLog "Info" $ "Start at " ++ show currms
        ++ " to search: " ++ show tim ++ " / " ++ show tpm ++ " / " ++ show mtg
        ++ " - maximal " ++ show dpt ++ " plys"
    modifyChanging $ \c -> c { working = True, srchStrtMs = currms,
                               crtStatus = posNewSearch (crtStatus c) }
    ctx <- ask
    tid <- newThread (startSearchThread tim tpm mtg dpt)
    modifyChanging (\c -> c { compThread = Just tid })
    return ()

startSearchThread :: Int -> Int -> Int -> Int -> CtxIO ()
startSearchThread tim tpm mtg dpt = do
    fd <- getIParamDef "firstDepth" 1
    ctxCatch (searchTheTree fd dpt 0 tim tpm mtg Nothing [] [])
        $ \e -> do
            let mes = "searchTheTree terminated by exception: " ++ show e
            ctx <- ask
            case logger ctx of
                Just _  -> ctxLog "Error" mes
                Nothing -> do
                    let efname = "Abulafia_" ++ show (strttm ctx) ++ "_err.txt"
                        efcont = unlines [idName, mes]
                    liftIO $ writeFile efname efcont
            answer $ infos mes
            liftIO $ threadDelay $ 50*1000 -- give time to send the ans

-- ctxCatch :: CE.Exception e => CtxIO a -> (e -> CtxIO a) -> CtxIO a
ctxCatch :: CtxIO a -> (CE.SomeException -> CtxIO a) -> CtxIO a
ctxCatch a f = do
    ctx <- ask
    liftIO $ CE.catch (runReaderT a ctx)
            (\e -> runReaderT (f e) ctx)

internalStop :: Int -> CtxIO ()
internalStop ms = do
    let sleep = ms * 1000
    ctxLog "Debug" $ "Internal stop clock started for " ++ show ms ++ " ms"
    liftIO $ threadDelay sleep
    ctxLog "Debug" "Internal stop clock ended"
    doStop False

betterSc = 25

-- Search with the given depth
searchTheTree :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe Int -> [Move] -> [Move] -> CtxIO ()
searchTheTree tief mtief timx tim tpm mtg lsc lpv rmvs = do
    chg <- readChanging
    ctxLog "Info" $ "Time = " ++ show tim ++ " Timx = " ++ show timx
    (path, sc, rmvsf, stfin) <- bestMoveCont tief timx (crtStatus chg) lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness!
    storeBestMove path sc	-- write back in status
    modifyChanging (\c -> c { crtStatus = stfin })
    currms <- lift currMilli
    let (ms', mx) = compTime tim tpm mtg sc
        ms  = if sc > betterSc
                 then ms' * 4 `div` 5
                 else if sc < -betterSc
                      then ms' * 6 `div` 5
                      else ms'
        strtms = srchStrtMs chg
        delta = strtms + ms - currms
        ms2 = ms `div` 2
        onlyone = ms > 0 && length rmvsf == 1 && tief >= 4	-- only in normal play
        timeover = ms > 0 && delta <= ms2  -- time is half over
        depthmax = tief >= mtief	--  or maximal depth
        mes = "Depth " ++ show tief ++ " Score " ++ show sc ++ " in ms "
                ++ show currms ++ " remaining " ++ show delta
                ++ " path " ++ show path
    -- answer $ infos $ "currms = " ++ show currms
    -- answer $ infos $ "ms     = " ++ show ms
    -- answer $ infos $ "mx     = " ++ show mx
    -- answer $ infos $ "cr+mx  = " ++ show (currms + mx)
    ctxLog "Info" mes
    ctxLog "Info" $ "compTime: " ++ show ms' ++ " / " ++ show mx
    -- if ms > 0 && (delta <= 0 || tief >= mtief)  -- time is over or maximal depth
    if depthmax || timeover || onlyone
        then do
            -- answer $ infos $ "End of search"
            -- answer $ infos $ "depthmax = " ++ show depthmax
            -- answer $ infos $ "timeover = " ++ show timeover
            -- answer $ infos $ "onlyone = " ++ show onlyone
            when depthmax $ ctxLog "Info" "in searchTheTree: max depth reached"
            giveBestMove path
        else do
            chg <- readChanging
            if working chg
                then searchTheTree (tief + 1) mtief (currms + mx) tim tpm mtg (Just sc) path rmvsf
                else do
                    ctxLog "Info" "in searchTheTree: not working"
                    giveBestMove path -- was stopped

storeBestMove :: [Move] -> Int -> CtxIO ()
storeBestMove mvs sc = do
    let s = InfoB { infoPv = mvs, infoScore = sc }
    modifyChanging (\c -> c { forGui = Just s })

giveBestMove :: [Move] -> CtxIO ()
giveBestMove mvs = do
    -- ctxLog "Info" $ "The moves: " ++ show mvs
    modifyChanging $ \c -> c {
        working = False, compThread = Nothing, forGui = Nothing }
    if null mvs
        then answer $ infos "empty pv"
        else answer $ bestMove (head mvs) Nothing

beforeReadLoop :: CtxIO ()
beforeReadLoop = do
    chg <- readChanging
    let evst1 = evalst $ crtStatus chg
        evst2 = evalst $ nxtStatus chg
    ctxLog "Info" "Eval parameters player 1 (" ++ crtEvpid chg ++ "):"
    forM_ (zip paramNames (esDParams evst1)) $ \(n, v) -> ctxLog "Info" $! n ++ "\t" ++ show v
    ctxLog "Info" "Eval parameters player 2 (" ++ nxtEvpid chg ++ "):"
    forM_ (zip paramNames (esDParams evst2)) $ \(n, v) -> ctxLog "Info" $! n ++ "\t" ++ show v

beforeProgExit :: CtxIO ()
beforeProgExit = when learnEval $ do
    chg <- readChanging
    let evst = evalst $ crtStatus chg
    lift $ do
        crtt <- currentSecs
        let newEvalFile = learnConfigFilePrefix ++ show crtt ++ ".txt"
        writeFile newEvalFile $
           unlines $ map (\(n, v) -> n ++ " = " ++ show v) $ zip paramNames $ esDParams evst

doStop extern = do
    chg <- readChanging
    modifyChanging (\c -> c { working = False, compThread = Nothing })
    case compThread chg of
        Just tid -> do
            -- when extern $ liftIO $ threadDelay 500000  -- warte 0.5 Sec.
            when extern $ liftIO $ threadDelay 100000  -- warte 0.1 Sec.
            liftIO $ killThread tid
            case forGui chg of
                Just ifg -> giveBestMove $ infoPv ifg
                Nothing  -> return ()
        _ -> return ()

doPonderhit = notImplemented "doPonderhit"

-- Helper: Antwortet dem GUI mit dem gegebenen String
answer :: String -> CtxIO ()
answer s = do
    ctx <- ask
    liftIO $ writeChan (writer ctx) s

-- Version and suffix:
progVersion = "0.60"
progVerSuff = ""

progLogName = "abulafia" ++ "-" ++ progVersion
                 ++ if null progVerSuff then ""
                                        else "-" ++ progVerSuff

-- These are the possible answers from engine to GUI:
idName = "id name Abulafia " ++ progVersion
             ++ if null progVerSuff then "" else " " ++ progVerSuff
idAuthor = "id author Nicu Ionita"
uciOk = "uciok"

readyOk = "readyok"

bestMove m mp = s
    where s = "bestmove " ++ toString m ++ sp
          sp = maybe "" (\v -> " ponder " ++ toString v) mp

-- Info Antworten:
-- sel.depth nicht implementiert
formInfo :: InfoToGui -> String
formInfo itg = "info"
    -- ++ " score cp " ++ show isc
    ++ formScore isc
    ++ " depth " ++ show (infoDepth itg)
    -- ++ " seldepth " ++ show idp
    ++ " time " ++ show (infoTime itg)
    ++ " nodes " ++ show (infoNodes itg)
    ++ nps
    ++ " pv" ++ concatMap (\m -> ' ' : toString m) (infoPv itg)
    where nps = case infoTime itg of
                0 -> ""
                x -> " nps " ++ show (infoNodes itg `div` x * 1000)
          isc = infoScore itg

formInfoB :: InfoToGui -> String
formInfoB itg = "info"
    -- ++ " score cp " ++ show isc
    ++ formScore isc
    ++ " pv" ++ concatMap (\m -> ' ' : toString m) (infoPv itg)
    where isc = infoScore itg

formScore i
    | i >= mateScore - 255    = " score mate " ++ show ((mateScore - i + 1) `div` 2)
    | i <= (-mateScore) + 255 = " score mate " ++ show ((-mateScore - i) `div` 2)
    | otherwise               = " score cp " ++ show i

-- sel.depth nicht implementiert
formInfo2 :: InfoToGui -> String
formInfo2 itg = "info"
    ++ " depth " ++ show (infoDepth itg)
    ++ " time " ++ show (infoTime itg)
    ++ " nodes " ++ show (infoNodes itg)
    ++ nps
    -- ++ " pv" ++ concatMap (\m -> ' ' : toString m) (infoPv itg)
    where nps = case infoTime itg of
                0 -> ""
                x -> " nps " ++ show (infoNodes itg * 1000 `div` x)

formInfoNps itg
    = case infoTime itg of
          0 -> Nothing
          x -> Just $ "info nps " ++ show (infoNodes itg `div` x * 1000)

formInfoDepth itg
    = "info depth " ++ show (infoDepth itg)
      --  ++ " seldepth " ++ show (infoDepth itg)

formInfoCM itg
    = "info currmove " ++ toString (infoMove itg)
        ++ " currmovenumber " ++ show (infoCurMove itg)

depth d sd = "info depth " ++ show d

inodes n = "info nodes " ++ show n

pv t mvs = "info time " ++ show t ++ " pv"
    ++ concatMap (\m -> ' ' : toString m) mvs

nps n = "info nps " ++ show n

infos s = "info string " ++ s
