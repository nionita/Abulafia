{-# LANGUAGE TypeSynonymInstances,
             FlexibleInstances
  #-}

module Main where

import Prelude hiding (catch)
import Control.Monad (when, forM_, mapM, void)
import Control.Monad.State
import Control.Monad.Reader
import Control.Exception (catch)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Maybe (fromJust, fromMaybe)
import Data.List (isPrefixOf)
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import System.IO
import System.Process
import System.Console.GetOpt
import System.Time

import Struct.Struct
import Struct.Status
import Moves.Moves
import Moves.Board
import Moves.BaseTypes
import Moves.Base
import Moves.History
import Eval.Eval
import Hash.TransTab
import Search.AlbetaTypes
import Search.Albeta
-- import Search.SearchA
import Search.SearchMonad
import Config.ConfigClass
import Config.Config
-- import Moves.Notation

-- There is some duplicated code here mostly from FixPlayFen.hs and Eval.hs
-- which could be eliminated by factorising

data Options = Options {
         optVerbose :: Bool,		-- what else?
         optIdent   :: Maybe String,	-- walk session identifier
         optFen     :: Maybe String,	-- analyse with FixPlayFen
         optDepth   :: Int,		-- analyse depth (when fen is given)
         optSDepth  :: Int,		-- max show depth
         optRoot    :: Int		-- visible tree root node
     }

defaultOptions = Options {
        optVerbose = False,
        optIdent   = Nothing,
        optFen     = Nothing,
        optDepth   = 10,
        optSDepth  = 2,
        optRoot    = 0
    }

setVerbose opt   = opt { optVerbose = True }
setIdent   opt v = opt { optIdent   = Just v }
setFen     opt v = opt { optFen     = Just v }
setDepth   opt v = opt { optDepth   = v }
setSDepth  opt v = opt { optSDepth  = v }
setRoot    opt v = opt { optRoot    = v }

options :: [OptDescr (Options -> Options)]
options = [
        Option ['i'] ["ident"]   (ReqArg (flip setIdent) "STRING")
            "identifier for the tree walk session",
        Option ['f'] ["fen"] (ReqArg (flip setFen) "FEN")
            "fen to be analysed to produce the tree",
        Option ['d'] ["depth"]   (ReqArg (flip setDepth . read) "INT")
            "depth for analysis",
        Option ['s'] ["show"]    (ReqArg (flip setSDepth . read) "INT")
            "visible tree depth",
        Option ['r'] ["root"]    (ReqArg (flip setRoot . read) "INT")
            "visible tree root",
        Option ['v'] ["verbose"] (NoArg setVerbose)
            "enable verbose"
    ]

vizOptions :: [String] -> IO (Options, [String])
vizOptions argv = case getOpt Permute options argv of
    (o, n, [])   -> return (foldr ($) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: VizGraph -i IDENT [-f FEN [-d INT]] [-s INT] [-r INT] [-v]"

main = plotWithOptions

plotWithOptions = do
    args <- getArgs
    (opts, _) <- vizOptions args
    case optIdent opts of
        Nothing -> ioError (userError (usageInfo error1 options))
        Just se -> do
            case optFen opts of
                Just fen -> plotFromFen  opts se fen
                Nothing  -> plotFromFile opts se
            drawAndShow opts
    where error1 = "You must provide an identifier for the walk session"

plotFromFen  opts sess fen = do
    dex <- doesDirectoryExist sess
    if dex
       then ioError (userError $ "Directory for new session " ++ sess ++ " already exist! "
                                   ++ "You cannot provide a fen for an older session")
       else do
           createDirectory sess
           setCurrentDirectory sess
           procPlot opts fen

plotFromFile opts sess = do
    when (optVerbose opts) $ putStrLn $ "Start plotFromFile"
    dex <- doesDirectoryExist sess
    if not dex
       then ioError (userError $ "Directory for session " ++ sess ++ " does no exist! "
                                    ++ "For new session you must provide a fen")
       else do
           setCurrentDirectory sess
           filePlot opts

-- Here is how to find the analysing program
base = "J:/AbaAba"
build = "dist" </> "build"

analyseFile = "analyse.txt"
fenFile     = "fen.txt"

-- First time processing in this session: search the fen
-- and write the log to a file, then process from file
procPlot :: Options -> String -> IO ()
procPlot opts fen = do
    writeFile fenFile fen
    sh <- openFile analyseFile WriteMode
    runDepthFen (optDepth opts) fen sh
    hClose sh
    filePlot opts

-- Processing from file
filePlot :: Options -> IO ()
filePlot opts = do
    oh <- openFile analyseFile ReadMode `catch` printError
    procInput oh opts

printError :: IOError -> IO a
printError e = putStrLn errm >> return undefined
    where errm = "Error: " ++ show e

procInput ih opts = do
    when (optVerbose opts) $ putStrLn $ "Start procInput"
    evalStateT go state0
    where go = do
              eof <- lift $ hIsEOF ih
              if eof
                  then closeDot
                  else do
                      line <- lift $ B.hGetLine ih
                      when (optVerbose opts) $ liftIO $ putStrLn $ "Read next line: " ++ B.unpack line
                      dispatch opts line
                      go

data GNode = GNode {
                ndNumb  :: Int,
                ndDIntv :: [ByteString],
                ndScore :: Maybe ByteString,
                ndLeaf  :: Bool,
                ndRese  :: Bool,
                ndBCut  :: Bool
            } deriving Show

data Phase = Pre | In | Post deriving Eq	-- before, during or after the visible tree

data DState = DState {
                   stOpened  :: Bool,
                   stPhase   :: Phase,
                   stOFile   :: Handle,
                   stCounter :: !Int,
                   stStack   :: [GNode],
                   stPly     :: !Int,
                   stVizRoot :: !Int,
                   stVizPly  :: !Int
               }

mkRoot d = GNode { ndNumb = 0, ndDIntv = [B.pack $ d ++ ":(-inf,+inf)"],
                  ndScore = Nothing, ndLeaf = False, ndRese = False, ndBCut = False }
state0 = DState { stOpened = False, stPhase = Pre, stOFile = undefined,
                   stCounter = 1, stStack = [], stPly = 0, stVizRoot = 0, stVizPly = 0 }

type Dotter = StateT DState IO

dispatch :: Options -> ByteString -> Dotter ()
dispatch opts line = case B.break isSpace line of	-- break on first space
    (verb,  rest) | verb == new  -> updateNew  opts $ B.drop 1 rest
                  | verb == down -> updateDown opts $ B.drop 1 rest
                  | verb == up   -> updateUp   opts $ B.drop 1 rest
                  | verb == abd  -> updateABD  opts $ B.drop 1 rest
                  | verb == rese -> updateRese opts $ B.drop 1 rest
                  | verb == sco  -> updateSco  opts $ B.drop 1 rest
                  | otherwise    -> return ()
    where new  = B.pack "NEW"
          down = B.pack "DOWN"
          up   = B.pack "UP"
          abd  = B.pack "ABD"
          rese = B.pack "RESE"
          sco  = B.pack "SCO"

updateNew :: Options -> ByteString -> Dotter ()
updateNew opts sdepth = do
    let idepth = read $ B.unpack sdepth
    s <- get
    when (stOpened s) closeDot
    when (idepth == optDepth opts) $ do
        let rootnode = mkRoot (show $ optDepth opts)
            stk = [rootnode]
        when (optVerbose opts) $ liftIO $ putStrLn $ "Open dot file " ++ show (dotFile opts)
        h <- lift $ openFile (dotFile opts) WriteMode
        when (optVerbose opts) $ liftIO $ putStrLn $ "Dot file " ++ show (dotFile opts) ++ " opened"
        if optRoot opts == 0
           then do
               -- write beginning of dot file and describe node 0
               lift $ do
                   dotHeader h
                   descNodeRoot h
               put state0 { stOpened = True, stPhase = In,  stOFile = h, stStack = stk }
           else
               put state0 { stOpened = True, stPhase = Pre, stOFile = h, stStack = stk }

updateDown :: Options -> ByteString -> Dotter ()
updateDown opts bs = do
    s <- get
    when (stOpened s) $ do
        let nn = maybe (-1) fst $ B.readInt bs
            node = GNode { ndNumb = nn, ndDIntv = [], ndScore = Nothing,
                          ndLeaf = True, ndRese = False, ndBCut = False }
            (pnode : sstk) = stStack s
            nPly = stPly s + 1
            vPly = if stPhase s == In then stVizPly s + 1 else stVizPly s
            stk  = if ndLeaf pnode
                      then node : pnode { ndLeaf = False } : sstk
                      else node : stStack s
        if stPhase s == Pre && ndNumb node == optRoot opts
           then do
               lift $ dotHeader (stOFile s)
               put s { stCounter = stCounter s + 1, stStack = stk, stPly = nPly,
                       stPhase = In, stVizRoot = nn }
           else put s { stCounter = stCounter s + 1, stStack = stk, stPly = nPly,
                        stVizPly = vPly }

updateUp :: Options -> ByteString -> Dotter ()
updateUp opts bs = do
    s <- get
    when (stOpened s) $ do
        when (null $ stStack s) $ liftIO $ ioError $ userError $ "Up: empty stStack: " ++ show bs
        let (node : stk) = stStack s
        when (null stk) $ liftIO $ ioError $ userError $ "Up: no parent for " ++ show node
        let parent = head stk
            (snn : move : score : _) = B.words bs
            nPly = stPly s - 1
            vPly = if stPhase s == In then stVizPly s - 1 else stVizPly s
            snn' = B.unpack snn
        when (read snn' /= ndNumb node) $ liftIO
            $ ioError (userError $ "Up: wrong node: " ++ snn' ++ ", expect " ++ show (ndNumb node))
        when (stPhase s == In && stVizPly s <= optSDepth opts) $ do
            -- write the new node and the relation to the parent
            -- nodes on the frontier are represented differently
            let descNodeA = if stVizPly s == optSDepth opts && nodeHasChildren node
                               then descNodeFrontier
                               else descNodeNormal
            lift $ descNodeA (stOFile s) node move (stPly s)
            lift $ descEdge (stOFile s) parent (ndNumb node) move score
        if vPly < 0
           then put s { stPhase = Post, stStack = stk, stPly = nPly, stVizPly = vPly }
           else put s {                 stStack = stk, stPly = nPly, stVizPly = vPly }

updateABD :: Options -> ByteString -> Dotter ()
updateABD opts str = do
    s <- get
    when (stOpened s) $ do
        let (node : stk) = stStack s
            (a:b:d:_)    = B.words str
            dab          = B.concat [d,colo,left,a,coma,b,right]
            node' = node { ndDIntv = dab : ndDIntv node }
        put s { stStack = node' : stk }
    where left  = B.pack "("
          right = B.pack ")"
          coma  = B.pack ","
          colo  = B.pack ":"

updateRese :: Options -> ByteString -> Dotter ()
updateRese opts _ = do
    s <- get
    when (stOpened s) $ do
        let (node : stk) = stStack s
            node' = node { ndRese = True }
        put s { stStack = node' : stk }

updateSco :: Options -> ByteString -> Dotter ()
updateSco opts scorebs = do
    s <- get
    when (stOpened s) $ do
        let (node : stk) = stStack s
            node' = node { ndScore = Just scorebs }
        put s { stStack = node' : stk }

nodeHasChildren = not . ndLeaf

closeDot :: Dotter ()
closeDot = do
    s <- get
    lift $ hPutStr (stOFile s) "}"	-- closing bracket
    lift $ hClose $ stOFile s 	-- should we check consistence on this close?
    put s { stOpened = False }

dotHeader h = do
    hPutStrLn h "digraph pos {"
    hPutStrLn h "\tratio=auto"
    -- hPutStrLn h "\tsize=\"7.5,10\""
    -- hPutStrLn h "\tnode [shape=circle,fontsize=10]"
    hPutStrLn h "\tnode [shape=box,fontsize=10]"

descNodeRoot h = hPutStrLn h "\t0 [color=green,label=\"root\"]"	--,color=green]

descNodeFrontier :: Handle -> GNode -> ByteString -> Int -> IO ()
descNodeFrontier h node move ply =
    -- hPutStrLn h $ "\t" ++ sn ++ " [style=filled,color=gray,label=\""
    -- hPutStrLn h $ "\t" ++ sn ++ " [shape=doublecircle,color=" ++ col ++ ",label=\""
    hPutStrLn h $ "\t" ++ sn ++ " [peripheries=2,color=" ++ col ++ ",label=\""
                       ++ label node move ply ++ "\"]"
    where col = if even ply then "green" else "red"
          sn   = show $ ndNumb node

descNodeNormal :: Handle -> GNode -> ByteString -> Int -> IO ()
descNodeNormal h node move ply =
    hPutStrLn h $ "\t" ++ sn ++ " [color=" ++ col ++ ",label=\""
                       ++ label node move ply ++ "\"]"
    where col = if even ply then "green" else "red"
          sn   = show $ ndNumb node

descEdge h parent nn move score =
    hPutStrLn h $ "\t" ++ parnum ++ " -> " ++ show nn
                       ++ " [label=\"" ++ B.unpack move ++ "/" ++ B.unpack score ++ "\"" ++ color ++ "]"
    where color = if ndRese parent then ",color=red" else ""
          parnum = show . ndNumb $ parent

label node move ply = foldl center base $ reverse $ ndDIntv node
    where sn   = show $ ndNumb node
          -- base = B.unpack move ++ " [" ++ sn ++ "/" ++ show ply ++ "]" ++ sco
          base = "[" ++ sn ++ "/" ++ show ply ++ "]" ++ sco
          sco  = maybe "" (\s -> "\\n" ++ B.unpack s) $ ndScore node
          center a b = a ++ "\\n" ++ B.unpack b

drawAndShow opts = do
    fex <- doesFileExist $ dotFile opts
    if not fex
        then putStrLn "No dot file created (perhaps depth?)"
        else do
            system $ "dot -T" ++ format ++ " -o " ++ outFile ++ " " ++ dotFile opts
            void $ runCommand outFile
    where format = "svg"
          outFile = forFile opts format

varFile opts = "root-" ++ show (optRoot opts)
dotFile opts = varFile opts ++ ".dot"
forFile opts format = varFile opts ++ "." ++ format


-- Here we work in reader transformer over the IO monad
-- The context is the open file handler where we write all
-- messages from the search

type LoggerIO = ReaderT Handle IO

instance CtxMon LoggerIO where
    tellCtx = tellToFile
    timeCtx = return 0	-- we are not interested in any time contro

runDepthFen depth fen oh = do
    let pos = updatePos $ posFromFen fen
    putStrLn $ "Analyse depth " ++ show depth ++ " fen " ++ fen
    ha  <- newCache defaultConfig
    hi  <- newHist
    evs <- makeEvalState Nothing
    TOD s0 ps0 <- getClockTime
    let inist = posToState pos ha hi evs
    n <- runReaderT (searchTheTree 1 depth inist Nothing [] []) oh
    TOD s1 ps1 <- getClockTime
    let ttime = fromIntegral (s1 - s0) + fromIntegral (ps1 - ps0) / 10^12
    putStrLn ""
    putStrLn $ "Total time (secs): " ++ show ttime
    putStrLn $ "Total nps        : " ++ show (round $ fromIntegral n / ttime)

-- This is used to redirect the log messages from the search to a file
-- We are interested only in the log messages (no best move and such)
-- and we filter directly only the ones needed for visualisation
tellToFile :: Comm -> LoggerIO ()
tellToFile (LogMes s) = do
    h <- ask
    when (linepfx `isPrefixOf` s) $ lift $ hPutStrLn h $ drop pfxlen s
    return ()
    where linepfx = "***"	-- To filter only visualisation relevand log lines
          pfxlen  = length linepfx
tellToFile _ = return ()

-- Parameter of the search at this level
aspirWindow   = 16	-- initial aspiration window
showEvalStats = False	-- show eval statistics in logfile

-- One iteration in the search for the best move
bestMoveCont :: Int -> MyState -> Maybe Int -> [Move] -> [Move] -> LoggerIO ([Move], Int, [Move], MyState)
bestMoveCont tiefe stati lastsc lpv rmvs = do
    -- informGuiDepth tief
    -- ctxLog "Info" $ "start search for depth " ++ show tief
    let abc = ABC {
                maxdepth = tiefe,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                best      = True,
                stoptime  = 0
                }
    ((sc, path, rmvsf), statf) <- runSearch (alphaBeta abc) stati
    when (sc == 0) $ return ()
    let n = nodes . stats $ statf
    tellToFile (BestMv sc tiefe n path)
    return (path, sc, rmvsf, statf)

searchTheTree :: Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> LoggerIO Int
searchTheTree tief mtief mystate lsc lpv rmvs = do
    -- search with the given dept
    (path, sc, rmvsf, stfin) <- bestMoveCont tief mystate lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness
    if tief >= mtief  -- maximal dept
        then giveBestMove path (nodes $ stats stfin)
        else searchTheTree (tief + 1) mtief stfin (Just sc) path rmvs

giveBestMove :: [Move] -> Int -> LoggerIO Int
giveBestMove mvs nodes = do
    lift $ putStr $ case mvs of
        (fmv:_) -> " -> bm " ++ show fmv ++ ", ns " ++ show nodes
        _       -> " -> bm empty PV (" ++ show nodes ++ " nodes)"
    return nodes

-- Opens a parameter file for eval, read it and create an eval stat
makeEvalState argfile  =
    case argfile of
        Just afn -> do
            fex <- doesFileExist afn
            if fex then filState afn else defState
        Nothing -> defState
    where defState = return $ initEvalState []
          filState fn = fmap initEvalState (fileToParams `fmap` readFile fn)

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
