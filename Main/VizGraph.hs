module Main where

import Control.Monad (when, forM_, mapM)
import Control.Monad.State
import Data.Char (isSpace)
import Data.Maybe (fromJust, fromMaybe)
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import System.IO
import System.Process
import System.Console.GetOpt

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

-- Processing from pipe
procPlot :: Options -> String -> IO ()
procPlot opts fen = do
    let fixFen  = base </> build </> "FixPlayFen" </> "FixPlayFen.exe"
        currDir = base </> build </> "Abulafia"
        pp = (proc fixFen [show (optDepth opts), fen]) { std_out = CreatePipe }
    (_, mbout, _, _) <- createProcess pp
    case mbout of
        Nothing -> ioError $ userError $ "Cannot start process:\n" ++ fixFen
                                       ++ "\nin directory " ++ show currDir
        Just oh -> do
            writeFile fenFile fen
            sh <- openFile analyseFile WriteMode
            procInput oh (Just sh) opts

-- Processing from file
filePlot :: Options -> IO ()
filePlot opts = do
    oh <- openFile analyseFile ReadMode
    procInput oh Nothing opts

procInput ih msh opts = do
    evalStateT go state0
    where go = do 
              eof <- lift $ hIsEOF ih
              if eof
                  then closeDot >> maybe (return ()) (\h -> lift $ hClose h) msh
                  else do
                      line <- lift $ hGetLine ih
                      case lineok line of
                          Just l  -> do
                              maybe (return ()) (\h -> lift $ hPutStrLn h line) msh
                              dispatch opts l
                          Nothing -> return ()
                      go

lineok :: String -> Maybe String
lineok line = case line of
    ('L':'o':'g':':':' ':'*':'*':'*':_) -> Just (drop 8 line) 
    _                                   -> Nothing

data Node = Node {
                ndNumb :: Int,
                ndAlpha, ndBeta :: Maybe Int,
                ndDepth :: Maybe Int,
                ndScore :: Maybe Int,
                ndLeaf  :: Bool,
                ndBCut  :: Bool
            }

data Phase = Pre | In | Post deriving Eq	-- before, during or after the visible tree

data MyState = MyState {
                   stOpened  :: Bool,
                   stPhase   :: Phase,
                   stOFile   :: Handle,
                   stCounter :: !Int,
                   stStack   :: [Node],
                   stPly     :: !Int,
                   stVizRoot :: !Int,
                   stVizPly  :: !Int
               }

mkRoot d = Node { ndNumb = 0, ndAlpha = Just minBound, ndBeta = Just maxBound,
                  ndDepth = Just d, ndScore = Nothing, ndLeaf = False, ndBCut = False }
state0 = MyState { stOpened = False, stPhase = Pre, stOFile = undefined,
                   stCounter = 1, stStack = [], stPly = 0, stVizRoot = 0, stVizPly = 0 }

type Dotter = StateT MyState IO

dispatch :: Options -> String -> Dotter ()
dispatch opts line = case break isSpace line of
    ("NEW",  rest) -> updateNew  opts $ drop 1 rest
    ("DOWN", rest) -> updateDown opts $ drop 1 rest
    ("UP",   rest) -> updateUp   opts $ drop 1 rest

updateNew :: Options -> String -> Dotter ()
updateNew opts sdepth = do
    let idepth = read sdepth
    s <- get
    when (stOpened s) closeDot
    when (idepth == optDepth opts) $ do
        let rootnode = mkRoot (optDepth opts)
            stk = [rootnode]
        h <- lift $ openFile (dotFile opts) WriteMode
        if optRoot opts == 0
           then do
               -- write beginning of dot file and describe node 0
               lift $ do
                   dotHeader h
                   descNodeRoot h
               put state0 { stOpened = True, stPhase = In,  stOFile = h, stStack = stk }
           else
               put state0 { stOpened = True, stPhase = Pre, stOFile = h, stStack = stk }

updateDown :: Options -> String -> Dotter ()
updateDown opts _ = do
    s <- get
    when (stOpened s) $ do
        let node = Node { ndNumb = stCounter s, ndAlpha = Nothing, ndBeta = Nothing,
                          ndDepth = Nothing, ndScore = Nothing, ndLeaf = True, ndBCut = False }
            (pnode : sstk) = stStack s
            nPly = stPly s + 1
            vPly = if stPhase s == In then stVizPly s + 1 else stVizPly s
            stk  = if ndLeaf pnode
                      then node : pnode { ndLeaf = False } : sstk
                      else node : stStack s
        if stPhase s == Pre && ndNumb node == optRoot opts
           then do
               lift $ dotHeader (stOFile s)
               --  hPutStrLn h "\t" ++ show n ++ " [label=\"root\"]"	--,color=green]
               put s { stCounter = stCounter s + 1, stStack = stk, stPly = nPly,
                       stPhase = In, stVizRoot = stCounter s }
           else put s { stCounter = stCounter s + 1, stStack = stk, stPly = nPly,
                        stVizPly = vPly }

updateUp :: Options -> String -> Dotter ()
updateUp opts str = do
    s <- get
    when (stOpened s) $ do
        let (move : score : _) = words str
            (node : stk) = stStack s
            parent = show . ndNumb . head $ stk
            nPly = stPly s - 1
            vPly = if stPhase s == In then stVizPly s - 1 else stVizPly s
        when (stPhase s == In && stVizPly s <= optSDepth opts) $ do
            -- write the new node and the relation to the parent
            let descNodeA = if stVizPly s == optSDepth opts && nodeHasChildren node
                               then descNodeFrontier
                               else descNodeNormal
            lift $ descNodeA (stOFile s) node move (stPly s)
            lift $ descEdge (stOFile s) parent (ndNumb node) score
        if vPly < 0
           then put s { stPhase = Post, stStack = stk, stPly = nPly, stVizPly = vPly }
           else put s {                 stStack = stk, stPly = nPly, stVizPly = vPly }

nodeHasChildren n = not $ ndLeaf n

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
    hPutStrLn h "\tnode [shape=circle,fontsize=10]"

descNodeRoot h = hPutStrLn h "\t0 [color=green,label=\"root\"]"	--,color=green]

descNodeFrontier :: Handle -> Node -> String -> Int -> IO ()
descNodeFrontier h node move ply =
    -- hPutStrLn h $ "\t" ++ sn ++ " [style=filled,color=gray,label=\""
    hPutStrLn h $ "\t" ++ sn ++ " [shape=doublecircle,color=" ++ col ++ ",label=\""
                       ++ move ++ " (" ++ sn ++ ")\\n" ++ show ply ++ "\"]"
    where sn  = show $ ndNumb node
          col = if even ply then "green" else "red"

descNodeNormal :: Handle -> Node -> String -> Int -> IO ()
descNodeNormal h node move ply =
    hPutStrLn h $ "\t" ++ sn ++ " [color=" ++ col ++ ",label=\""
                       ++ move ++ " (" ++ sn ++ ")\\n" ++ show ply ++ "\"]"
    where sn  = show $ ndNumb node
          col = if even ply then "green" else "red"

descEdge h parent nn score =
    hPutStrLn h $ "\t" ++ parent ++ " -> " ++ show nn
                       ++ " [label=" ++ score ++ "]"

drawAndShow opts = do
    system $ "dot -T" ++ format ++ " -o " ++ outFile ++ " " ++ dotFile opts
    runCommand outFile
    where format = "svg"
          outFile = forFile opts format

varFile opts = "root-" ++ show (optRoot opts)
dotFile opts = varFile opts ++ ".dot"
forFile opts format = varFile opts ++ "." ++ format
