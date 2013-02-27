module Main (main) where

-- This is the CLOP driver, i.e. this program is called by CLOP (Remi Coulom)
-- in order to play a game and announce the result
-- The program is called with following parameters
-- #1: processor id - a symbolic name, QLR could run the driver with different values (in parallel)
-- #2: seed (integer) - game number
-- #3: parameter id of first parameter (name)
-- #4: value of first parameter (float)
-- #5: parameter id of second parameter (optional)
-- #6: value of second parameter (optional)
-- ...
-- The driver should write the game outcome to its output:
-- W = win
-- L = loss
-- D = draw

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad (forM, forM_, liftM, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy
import Data.List (intersperse, isPrefixOf, sortBy, groupBy, delete)
import qualified Data.Map as M
import Data.Maybe
import Data.Ord (comparing)
import Data.Ratio
import Data.Typeable
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error hiding (catch)
import System.Process
import System.Time
import System.Timeout (timeout)
import System.Locale (defaultTimeLocale)
import System.Random
import Text.Printf

import Struct.Status (EvalState(..))
import Eval.FileParams (fileToState)
import Eval.Eval (paramNames)

data DriverConfig = DC {
        dcRefEngine, dcRefConfig :: String,	-- reference engine and config file
        dcChaEngine, dcChaConfigSrc :: String,	-- challenger engine and config prototype
        dcRefMoves, dcRefFixTm, dcRefSecPerMv :: String,	-- time for reference
        dcChaMoves, dcChaFixTm, dcChaSecPerMv :: String,	-- time for challenger
        dcRefProto, dcChaProto :: String	-- protocols (uci/)
     }

-- Some constants for the evolving environment
learnDir   = "J:\\Learn\\CLOP"
gamesDir   = "Games"
currentDir = "Current"

-- Some constants for playing one match
cuteChessDir = "J:\\Chess\\cutechess-cli-0.5.1-win32"
cuteChessCom = cuteChessDir ++ "\\cutechess-cli.exe"

-- We have to use the first parameter from CLOP twofold:
-- first part will be a subdirectory in which the CLOP learn session will run
-- second part will a unique identifier for the parallel process
-- The 2 parts are delimitted by an underscore "_"
main = do
    (proc : seed : params)  <- getArgs
    let dict = makeDict params
        (session, thread) = break (== '_') proc
    res <- runGame session thread seed dict
    putStrLn res

-- Take a list of param/values sequence (as strings) and structure it
-- in form [(param, value)]
makeDict :: [String] -> [(String, Double)]
makeDict ss = go Nothing ss []
    where go Nothing [] acc = acc
          go _       [] _   = error "Unmatched param/value sequence"
          go Nothing  (p:ps) acc = go (Just p) ps acc
          go (Just p) (v:ps) acc = go Nothing  ps ((p, read v):acc)

-- Read a configuration file for the CLOP learn session
-- prepare the environment (i.e. config file, log files, pgn file names)
-- and start a game using cutechess-cli
runGame :: String -> String -> String -> [(String, Double)] -> IO String
runGame session thread seed dict = do
    setCurrentDirectory $ baseDir session
    cdconf <- readDriverConfig
    let white = last seed `elem` "13579"
        (args, refdir, chadir, chaconf) = mkCutechessCommand cdconf session thread white
    createDirectoryIfMissing True refdir
    createDirectoryIfMissing True chadir
    mkConfigFile dict cdconf chaconf
    wbd <- oneMatch args
    case wbd of
        (1, 0, 0) -> if white then return "W" else return "L"
        (0, 1, 0) -> if white then return "L" else return "W"
        _         -> return "D"

baseDir :: String -> FilePath
baseDir session = learnDir </> session

-- Give names to the candidates
nameCandidates evn cycle = zip (map label [1..])
    where label i = evn ++ "-" ++ show cycle ++ "-" ++ show i ++ ".txt"

mkConfigFile dict cdconf fname = do
    est <- fileToState (dcChaConfigSrc cdconf)
    ctime <- getClockTime >>= toCalendarTime >>= return . calendarTimeToString
    writeFile fname $ showConfig dict (esDParams est) ("-- Candidate created on " ++ ctime)

showConfig dict cnf comm = comm ++ "\n" ++ lins
    where lins = unlines $ map reppar $ zip paramNames cnf
          reppar (n, v) = n ++ " = " ++ show (maybe v id (lookup n dict))

-- Return a list of parameters for the cutechess-cli command,
-- the 2 directories in which the engines run and the name of the challenger config file
-- (which must be created)
mkCutechessCommand :: DriverConfig -> String -> String -> Bool
                   -> ([String], FilePath, FilePath, FilePath)
mkCutechessCommand dcf session thread white
    = (args, refcurr, chacurr, chaconf)
    where common = [
              "-event", session,
              "-draw", "150", "100", "-resign", "4", "800", "-site", "Sixpack",
              "-pgnout", pgnout
              ]
          eng1 = [
              "-engine",
              "name=" ++ takeFileName (dcChaEngine dcf),
              "cmd=" ++ dcChaEngine dcf,
              "arg=" ++ chaconf,
              "dir=" ++ chacurr,
              "proto=" ++ dcChaProto dcf,
              "tc=" ++ chatime
              ]
          eng2 = [
              "-engine",
              "name=" ++ takeFileName (dcRefEngine dcf),
              "cmd=" ++ dcRefEngine dcf,
              "arg=" ++ dcRefConfig dcf,
              "dir=" ++ refcurr,
              "proto=" ++ dcRefProto dcf,
              "tc=" ++ reftime
              ]
          args = if white then common ++ eng1 ++ eng2 else common ++ eng2 ++ eng1
          pgnout = base </> ("thr" ++ thread ++ ".pgn")
          refcurr = base </> ("ref" ++ thread)
          reftime = "tc=" ++ dcRefMoves dcf ++ "/" ++ dcRefFixTm dcf ++ "+" ++ dcRefSecPerMv dcf
          chacurr = base </> ("cha" ++ thread)
          chaconf = chacurr </> "evalParams.txt"
          chatime = "tc=" ++ dcChaMoves dcf ++ "/" ++ dcChaFixTm dcf ++ "+" ++ dcChaSecPerMv dcf
          base = baseDir session

eventName :: Event -> String
eventName = id

-- eventDir :: Event -> String
-- eventDir = (outDir </>) . eventName

type Event = String
type Player = String

oneMatch :: [String] -> IO (Int, Int, Int)
oneMatch args = do
    (_, Just hout, _, ph)
            <- createProcess (proc cuteChessCom args) { std_out = CreatePipe }
    catch (everyLine hout (0, 0, 0) 1) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error in everyLine: " ++ es
        terminateProcess ph
        throwIO e

everyLine _ r 0 = return r
everyLine h r g = do
    lin <- hGetLine h
    -- when debug $ putStrLn $ "Got: " ++ lin
    let (r1, g1) = if "Score of" `isPrefixOf` lin
                      then (getScore lin, g-1)
                      else (r, g)
    everyLine h r1 g1

-- The line has the following structure:
-- Score of x vs y: a - b - c [prc] n
-- where x and y are the opponents, a = wins of x, b = wins of y, c = draws
getScore :: String -> (Int, Int, Int)
getScore
    = listToTrio
    . map (read . snd)
    . filter (even . fst)
    . zip [0..]
    . take 5
    . drop 5
    . words

listToTrio (x:y:z:_) = (x, y, z)

readDriverConfig = return DC {
        dcRefEngine = "J:\\AbaAba\\dist\\build\\Abulafia\\Abulafia_0_60_wl.exe",
        dcRefConfig = "J:\\AbaAba\\dist\\build\\Abulafia\\evalParams-0.60.txt",
        dcChaEngine = "J:\\AbaAba\\dist\\build\\Abulafia\\Abulafia_0_62_nnn.exe",
        -- dcChaEngine = "J:\\AbaAba\\dist\\build\\Abulafia\\Abulafia_0_62_codrr.exe",
        dcChaConfigSrc = "J:\\AbaAba\\dist\\build\\Abulafia\\test1-51-6.txt",
        -- dcRefMoves = "40", dcRefFixTm = "20", dcRefSecPerMv = "0.2",	-- time for reference
        -- dcChaMoves = "40", dcChaFixTm = "20", dcChaSecPerMv = "0.2",	-- time for challenger
        dcRefMoves = "40", dcRefFixTm = "10", dcRefSecPerMv = "0.2",	-- time for reference
        dcChaMoves = "40", dcChaFixTm = "10", dcChaSecPerMv = "0.2",	-- time for challenger
        dcRefProto = "uci", dcChaProto = "uci"	-- protocols (uci/)
     }

pairings :: [a] -> [(a,a)]
pairings [] = []
pairings (a:as) = zip (repeat a) as ++ pairings as

alternate :: Bool -> [(a,a)] -> [(a,a)]
alternate _ [] = []
alternate False (xy : xys) = xy : alternate True xys
alternate True ((x, y) : xys) = (y, x) : alternate False xys

uniform :: IO Double
uniform = getStdRandom (randomR (-1, 1))

-- Marsaglia polar method for normal standard distribution:
genNormal = do
    u <- uniform
    v <- uniform
    let s = u * u + v * v
        s2s = sqrt $ (-2) * log s / s
        x = u * s2s
        -- y = v * s2s
    if s >= 1 then genNormal else return x

-- Translate a normal standard distribution to a different mean and standard deviation
fitNormal mu sigma = genNormal >>= \x -> case mu + sigma * x of y -> return y
