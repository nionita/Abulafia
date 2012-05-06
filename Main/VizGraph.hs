module Main where

import Control.Monad (when, forM_, mapM)
import Control.Monad.State
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
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
                      line <- lift $ B.hGetLine ih
                      case lineok line of
                          Just l  -> do
                              maybe (return ()) (\h -> lift $ B.hPutStrLn h line) msh
                              dispatch opts l
                          Nothing -> return ()
                      go

lineok :: ByteString -> Maybe ByteString
lineok line = if log `B.isPrefixOf` line then Just (B.drop 8 line) else Nothing
    where log = B.pack "Log: ***"

data Node = Node {
                ndNumb  :: Int,
                ndDIntv :: [ByteString],
                ndScore :: Maybe ByteString,
                ndLeaf  :: Bool,
                ndRese  :: Bool,
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

mkRoot d = Node { ndNumb = 0, ndDIntv = [B.pack $ d ++ ":(-inf,+inf)"],
                  ndScore = Nothing, ndLeaf = False, ndRese = False, ndBCut = False }
state0 = MyState { stOpened = False, stPhase = Pre, stOFile = undefined,
                   stCounter = 1, stStack = [], stPly = 0, stVizRoot = 0, stVizPly = 0 }

type Dotter = StateT MyState IO

dispatch :: Options -> ByteString -> Dotter ()
dispatch opts line = case B.break isSpace line of	-- break on first space
    (verb,  rest) | verb == new  -> updateNew  opts $ B.drop 1 rest
                  | verb == down -> updateDown opts $ B.drop 1 rest
                  | verb == up   -> updateUp   opts $ B.drop 1 rest
                  | verb == abd  -> updateABD  opts $ B.drop 1 rest
                  | verb == rese -> updateRese opts $ B.drop 1 rest
                  | otherwise    -> return ()
    where new  = B.pack "NEW"
          down = B.pack "DOWN"
          up   = B.pack "UP"
          abd  = B.pack "ABD"
          rese = B.pack "RESE"

updateNew :: Options -> ByteString -> Dotter ()
updateNew opts sdepth = do
    let idepth = read $ B.unpack sdepth
    s <- get
    when (stOpened s) closeDot
    when (idepth == optDepth opts) $ do
        let rootnode = mkRoot (show $ optDepth opts)
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

updateDown :: Options -> ByteString -> Dotter ()
updateDown opts bs = do
    s <- get
    when (stOpened s) $ do
        let nn = maybe (-1) fst $ B.readInt bs
            -- node = Node { ndNumb = stCounter s, ndIntv = Nothing,
            node = Node { ndNumb = nn, ndDIntv = [], ndScore = Nothing,
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
        let (snn : move : score : _) = B.words bs
            (node : stk) = stStack s
            parent = head stk
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
            lift $ descEdge (stOFile s) parent (ndNumb node) score
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

descNodeFrontier :: Handle -> Node -> ByteString -> Int -> IO ()
descNodeFrontier h node move ply =
    -- hPutStrLn h $ "\t" ++ sn ++ " [style=filled,color=gray,label=\""
    -- hPutStrLn h $ "\t" ++ sn ++ " [shape=doublecircle,color=" ++ col ++ ",label=\""
    hPutStrLn h $ "\t" ++ sn ++ " [peripheries=2,color=" ++ col ++ ",label=\""
                       ++ label node move ply ++ "\"]"
    where col = if even ply then "green" else "red"
          sn   = show $ ndNumb node

descNodeNormal :: Handle -> Node -> ByteString -> Int -> IO ()
descNodeNormal h node move ply =
    hPutStrLn h $ "\t" ++ sn ++ " [color=" ++ col ++ ",label=\""
                       ++ label node move ply ++ "\"]"
    where col = if even ply then "green" else "red"
          sn   = show $ ndNumb node

descEdge h parent nn score =
    hPutStrLn h $ "\t" ++ parnum ++ " -> " ++ show nn
                       ++ " [label=" ++ B.unpack score ++ color ++ "]"
    where color = if ndRese parent then ",color=red" else ""
          parnum = show . ndNumb $ parent

label node move ply = foldl center base $ reverse $ ndDIntv node
    where sn   = show $ ndNumb node
          base = B.unpack move ++ " [" ++ sn ++ "/" ++ show ply ++ "]"
          center a b = a ++ "\\n" ++ B.unpack b

drawAndShow opts = do
    system $ "dot -T" ++ format ++ " -o " ++ outFile ++ " " ++ dotFile opts
    runCommand outFile
    where format = "svg"
          outFile = forFile opts format

varFile opts = "root-" ++ show (optRoot opts)
dotFile opts = varFile opts ++ ".dot"
forFile opts format = varFile opts ++ "." ++ format
