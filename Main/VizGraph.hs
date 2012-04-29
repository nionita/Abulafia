module Main where

-- import Control.Applicative
import Control.Monad (when, forM_, mapM)
import Control.Monad.State
import Data.Char (isSpace)
import Data.Maybe (fromJust, fromMaybe)
import System.FilePath
import System.Environment (getArgs)
import System.IO
import System.Process
import System.Console.GetOpt

data Options = Options {
         optVerbose :: Bool,		-- what else?
         optFen     :: Maybe String,	-- analyse with FixPlayFen
         optInFile  :: Maybe String,	-- input from file
         optOutFile :: Maybe String,	-- output to file (when fen is given)
         optDotFile :: Maybe String,	-- dot file name (must be always given)
         optDepth   :: Int,		-- analyse depth (when fen is given)
         optSDepth  :: Int,		-- max show depth
         optRoot    :: Int,		-- visible tree root node
         optPath    :: Maybe String	-- path to the visible tree root node
     }

defaultOptions = Options {
        optVerbose = False,
        optFen     = Nothing,
        optInFile  = Nothing,
        optOutFile = Nothing,
        optDotFile = Nothing,
        optDepth   = 10,
        optSDepth  = 2,
        optRoot    = 0,
        optPath    = Nothing
    }

setVerbose opt   = opt { optVerbose = True }
setFen     opt v = opt { optFen     = Just v }
setInFile  opt v = opt { optInFile  = Just v }
setOutFile opt v = opt { optOutFile = Just v }
setDotFile opt v = opt { optDotFile = Just v }
setDepth   opt v = opt { optDepth   = v }
setSDepth  opt v = opt { optSDepth  = v }
setRoot    opt v = opt { optRoot    = v }
setPath    opt v = opt { optPath    = Just v }

options :: [OptDescr (Options -> Options)]
options = [
        Option ['g'] ["generate"]  (ReqArg (flip setDotFile) "FILE")
            "name of the dot file to generate",
        Option ['a'] ["analyse"] (ReqArg (flip setFen) "FEN")
            "fen to be analysed to produce the tree",
        Option ['o'] ["output"]  (ReqArg (flip setOutFile) "FILE")
            "output file to save the tree description for later draws",
        Option ['i'] ["input"]   (ReqArg (flip setInFile) "FILE")
            "input file (tree description, produced by a previous analyse)",
        Option ['d'] ["depth"]   (ReqArg (flip setDepth . read) "INT")
            "depth for analysis",
        Option ['s'] ["show"]    (ReqArg (flip setSDepth . read) "INT")
            "visible tree depth",
        Option ['r'] ["root"]    (ReqArg (flip setRoot . read) "INT")
            "visible tree root",
        Option ['p'] ["path"]    (ReqArg (flip setPath) "MOVE,MOVE,...")
            "path from real root to visible root node",
        Option ['v'] ["verbose"] (NoArg setVerbose)
            "enable verbose"
    ]

vizOptions :: [String] -> IO (Options, [String])
vizOptions argv = case getOpt Permute options argv of
    (o, n, [])   -> return (foldr ($) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: VizGraph -g DOTFILE {-a FEN [-o FILE]|-i FILE} [-d INT] [-s INT] [{-r INT|-p PATH}] [-v]"

main = plotWithOptions

plotWithOptions = do
    args <- getArgs
    (opts, _) <- vizOptions args
    case optDotFile opts of
        Nothing -> ioError (userError error1)
        Just df -> do
            case optFen opts of
                Just fen -> plotFromFen fen opts
                Nothing  -> case optInFile opts of
                    Just inf -> plotFromFile inf opts
                    Nothing  -> ioError (userError (usageInfo error2 options))
            drawAndShow df opts
    where error1 = "You must provide a dot file name (-g)"
          error2 = "You must give either a fen or an input file"

plotFromFen  fen opts = procPlot fen opts

plotFromFile inf opts = filePlot inf opts

-- Here is how to find the analysing program
base = "J:/AbaAba"
build = "dist" </> "build"

-- Processing from pipe
procPlot :: String -> Options -> IO ()
procPlot fen opts = do
    let fixFen  = base </> build </> "FixPlayFen" </> "FixPlayFen.exe"
        currDir = base </> build </> "Abulafia"
        pp = (proc fixFen [show (optDepth opts), fen]) { std_out = CreatePipe }
    (_, mbout, _, _) <- createProcess pp
    case mbout of
        Nothing -> ioError $ userError $ "Cannot start process:\n" ++ fixFen
                                       ++ "\nin directory " ++ show currDir
        Just oh -> do
            msh <- case optOutFile opts of
                       Nothing -> return Nothing
                       Just f  -> do
                           sh <- openFile f WriteMode
                           return $ Just sh
            procInput oh msh opts

-- Processing from file
filePlot :: String -> Options -> IO ()
filePlot file opts = do
    oh <- openFile file ReadMode
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
                              dispath opts l
                          Nothing -> return ()
                      go

lineok :: String -> Maybe String
lineok line = case line of
    ('L':'o':'g':':':' ':'*':'*':'*':_) -> Just (drop 8 line) 
    _                                   -> Nothing

data Phase = Pre | In | Post deriving Eq	-- before, during or after the visible tree

data MyState = MyState {
                   stOpened  :: Bool,
                   stPhase   :: Phase,
                   stOFile   :: Handle,
                   stCounter :: !Int,
                   stStack   :: [Int],
                   stPly     :: !Int,
                   stVizRoot :: !Int,
                   stVizPly  :: !Int
               }

state0 = MyState { stOpened = False, stPhase = Pre, stOFile = undefined,
                   stCounter = 1, stStack = [0], stPly = 0, stVizRoot = 0, stVizPly = 0 }

type Dotter = StateT MyState IO

dispath :: Options -> String -> Dotter ()
dispath opts line = case break isSpace line of
    ("NEW",  rest) -> updateNew  opts $ drop 1 rest
    ("DOWN", rest) -> updateDown opts $ drop 1 rest
    ("UP",   rest) -> updateUp   opts $ drop 1 rest

updateNew :: Options -> String -> Dotter ()
updateNew opts sdepth = do
    let idepth = read sdepth
    s <- get
    when (stOpened s) closeDot
    when (idepth == optDepth opts) $ do
        h <- lift $ openFile (fromJust $ optDotFile opts) WriteMode
        if optPath opts == Nothing && optRoot opts == 0
           then do
               -- write beginning of dot file and describe node 0
               lift $ do
                   dotHeader h
                   descNodeRoot h
               put state0 { stOpened = True, stPhase = In,  stOFile = h }
           else
               put state0 { stOpened = True, stPhase = Pre, stOFile = h }

updateDown :: Options -> String -> Dotter ()
updateDown opts _ = do
    s <- get
    when (stOpened s) $ do
        let n    = stCounter s
            nPly = stPly s + 1
            vPly = if stPhase s == In then stVizPly s + 1 else stVizPly s
            stk  = n : stStack s
        if stPhase s == Pre && optPath opts == Nothing && n == optRoot opts
           then do
               lift $ dotHeader (stOFile s)
               --  hPutStrLn h "\t" ++ show n ++ " [label=\"root\"]"	--,color=green]
               put s { stCounter = n + 1, stStack = stk, stPly = nPly,
                       stPhase = In, stVizRoot = n }
           else put s { stCounter = n + 1, stStack = stk, stPly = nPly, stVizPly = vPly }

updateUp :: Options -> String -> Dotter ()
updateUp opts str = do
    s <- get
    when (stOpened s) $ do
        let (move : score : _) = words str
            (node : stk) = stStack s
            parent = show . head $ stk
            snode = show node
            nPly = stPly s - 1
            vPly = if stPhase s == In then stVizPly s - 1 else stVizPly s
        when (stPhase s == In && stVizPly s <= optSDepth opts) $ do
            -- write the new node and the relation to the parent
            if stVizPly s == optSDepth opts && nodeHadChildren node
               then lift $ descNodeFrontier (stOFile s) snode move (stPly s)
               else lift $ descNodeNormal   (stOFile s) snode move (stPly s)
            lift $ descEdge (stOFile s) parent snode score
        if vPly < 0
           then put s { stPhase = Post, stStack = stk, stPly = nPly, stVizPly = vPly }
           else put s {                 stStack = stk, stPly = nPly, stVizPly = vPly }

nodeHadChildren _ = False	-- dummy - we need node information!

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
    hPutStrLn h "\tnode [shape=circle]"

descNodeRoot h = hPutStrLn h "\t0 [label=\"root\"]"	--,color=green]

descNodeFrontier h node move ply =
    hPutStrLn h $ "\t" ++ node ++ " [style=filled,color=gray,label=\""
                       ++ move ++ " (" ++ node ++ ")\\n" ++ show ply ++ "\"]"

descNodeNormal h node move ply =
    hPutStrLn h $ "\t" ++ node ++ " [label=\""
                       ++ move ++ " (" ++ node ++ ")\\n" ++ show ply ++ "\"]"

descEdge h parent node score =
    hPutStrLn h $ "\t" ++ parent ++ " -> " ++ node
                       ++ " [label=" ++ score ++ "]"

drawAndShow dotf opts = do
    system $ "dot -T" ++ format ++ " -o " ++ outFile ++ " " ++ dotf
    runCommand outFile
    where format = "svg"
          outFile = dotf' ++ "." ++ format
          dotf' = reverse . drop 4 . reverse $ dotf
