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
         optFen     :: Maybe String,	-- analyse with FixPlayFen
         optInFile  :: Maybe String,	-- input from file
         optOutFile :: Maybe String,	-- output to file (when fen is given)
         optDepth   :: Int,		-- analyse depth (when fen is given)
         optSDepth  :: Int,		-- max show depth
         optRoot    :: Int,		-- tree draw root node
         optPath    :: Maybe String	-- path to the tree draw root node
     }

defaultOptions = Options {
        optFen     = Nothing,
        optInFile  = Nothing,
        optOutFile = Nothing,
        optDepth   = 10,
        optSDepth  = 3,
        optRoot    = 0,
        optPath    = Nothing
    }

setFen     opt v = opt { optFen     = Just v }
setInFile  opt v = opt { optInFile  = Just v }
setOutFile opt v = opt { optOutFile = Just v }
setDepth   opt v = opt { optDepth   = v }
setSDepth  opt v = opt { optSDepth  = v }
setRoot    opt v = opt { optRoot    = v }
setPath    opt v = opt { optPath    = Just v }

options :: [OptDescr (Options -> Options)]
options = [
        Option ['a'] ["analyse"] (ReqArg (flip setFen) "FEN")
            "fen to be analysed to produce the tree",
        Option ['i'] ["input"]   (ReqArg (flip setInFile) "FILE")
            "input file (tree description, produced by a previous analyse)",
        Option ['o'] ["output"]  (ReqArg (flip setOutFile) "FILE")
            "output file to save the tree description for later draws",
        Option ['d'] ["depth"]   (ReqArg (flip setDepth . read) "INT")
            "depth for analysis",
        Option ['s'] ["show"]    (ReqArg (flip setSDepth . read) "INT")
            "tree show depth",
        Option ['r'] ["root"]    (ReqArg (flip setRoot . read) "INT")
            "draw tree root",
        Option ['p'] ["path"]    (ReqArg (flip setPath) "MOVE,MOVE,...")
            "path from real root to draw root node"
    ]

vizOptions :: [String] -> IO (Options, [String])
vizOptions argv = case getOpt Permute options argv of
    (o, n, [])   -> return (foldr ($) defaultOptions o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: VizGraph [OPTION...]"

main = plotWithOptions

mainPlotGraph = do
    args <- getArgs
    case args of
        -- (depth : fen) -> toPlot (unwords fen) (read depth)
        (depth : fen : _) -> toPlot fen (read depth)
        _ -> putStrLn "Run with depth and fen!"

plotWithOptions = do
    args <- getArgs
    (opts, _) <- vizOptions args
    case optFen opts of
        Just fen -> plotFromFen fen opts
        Nothing  -> case optInFile opts of
                        Just inf -> plotFromFile inf opts
                        Nothing  -> ioError (userError (usageInfo header options))
    where header = "You must give either a fen or an input file"

plotFromFen fen opts = toPlot fen (optDepth opts)

plotFromFile inf opts = toPlot inf (optDepth opts)	-- fixme!

base = "J:/AbaAba"
build = "dist" </> "build"
filen = "vizg.dot"

toPlot :: String -> Int -> IO ()
toPlot fen depth = do
    let fixFen  = base </> build </> "FixPlayFen" </> "FixPlayFen.exe"
        currDir = base </> build </> "Abulafia"
    putStrLn $ "Showing graph for depth " ++ show depth ++ " fen " ++ fen
    (_, mbout, _, _) <- createProcess (proc fixFen [show depth, fen]) { std_out = CreatePipe }
    case mbout of
        Just oh -> procInput oh depth
        Nothing -> do
            putStrLn $ "Cannot start process " ++ fixFen
            putStrLn $ "in directory " ++ show currDir

procInput ih depth = do
    evalStateT go state0
    where go = do 
              eof <- lift $ hIsEOF ih
              if eof
                  then closeDot
                  else do
                      line <- lift $ hGetLine ih
                      case lineok line of
                          Just l  -> action depth l
                          Nothing -> lift $ putStrLn line
                      go

lineok :: String -> Maybe String
lineok line = case line of
    ('L':'o':'g':':':' ':'*':'*':'*':_) -> Just (drop 8 line) 
    _                                   -> Nothing

data MyState = MyState {
                   opened  :: Bool,
                   ofile   :: Handle,
                   counter :: !Int,
                   stack   :: [Int],
                   ply     :: !Int
               }

state0 = MyState { opened = False, ofile = undefined, counter = 0, stack = [], ply = 0 }

type Dotter = StateT MyState IO

action :: Int -> String -> Dotter ()
action depth line = case break isSpace line of
    ("NEW",  rest) -> updateNew depth $ drop 1 rest
    ("DOWN", rest) -> updateDown      $ drop 1 rest
    ("UP",   rest) -> updateUp        $ drop 1 rest

updateNew :: Int -> String -> Dotter ()
updateNew depth sdepth = do
    let idepth = read sdepth
    s <- get
    when (opened s) closeDot
    when (idepth == depth) $ do
        h <- lift $ openFile filen WriteMode
        -- write beginning of dot file and describe node 0
        lift $ hPutStrLn h "digraph pos {"
        lift $ hPutStrLn h "\tratio=auto"
        lift $ hPutStrLn h "\tsize=\"7.5,10\""
        lift $ hPutStrLn h "\tnode [shape=circle]"
        lift $ hPutStrLn h "\t0 [label=\"root\"]"	--,color=green]
        put MyState { opened = True, ofile = h, counter = 1, stack = [0], ply = 0 }

updateDown :: String -> Dotter ()
updateDown _ = do
    s <- get
    when (opened s) $ do
        let n = counter s
        put s { counter = n + 1, stack = n : stack s, ply = ply s + 1 }

updateUp :: String -> Dotter ()
updateUp str = do
    s <- get
    when (opened s) $ do
        let (move : score : _) = words str
            (node : stk) = stack s
            parent = show . head $ stk
            snode = show node
        -- write the new node and the relation to the parent
        lift $ hPutStrLn (ofile s) $ "\t" ++ snode ++ " [label=\""
                                  ++ move ++ " (" ++ snode ++ ")\\n" ++ show (ply s) ++ "\"]"
        lift $ hPutStrLn (ofile s) $ "\t" ++ parent ++ " -> " ++ snode
                                  ++ " [label=" ++ score ++ "]"
        put s { stack = stk, ply = ply s - 1 }

closeDot :: Dotter ()
closeDot = do
    s <- get
    lift $ hPutStr (ofile s) "}"	-- closing bracket
    lift $ hClose $ ofile s 	-- should we check consistence on this close?
    put s { opened = False }
