module Main (main) where

-- This is the QLR driver, i.e. this program is called by QLR (Remi Coulom)
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

-- Some constants for the evolving environment
playersDir = "Players"
gamesDir   = "Games"
currentDir = "Current"
statusFile = "status.txt"
statusFileN = "status-new.txt"
goonFile   = "running"

-- Some constants for playing one match
cuteChessDir = "J:\\Chess\\cutechess-cli-0.5.1-win32"
cuteChessCom = cuteChessDir ++ "\\cutechess-cli.exe"
engPath   = "J:\\AbaAba\\dist\\build\\Abulafia"
-- Big care here: the running engine should not produce a logfile,
-- as that would give an error when starting the same engine twice in the same directory
engine    = "Abulafia_0_62_nnn"
noGames   = 20
parGames  = "-rounds " ++ show noGames
tcMoves   = 40	-- moves
tcFixTm   = 20	-- seconds
secPerMv  = 0.2	-- second per move
parTime   = "tc=" ++ show tcMoves ++ "/" ++ show tcFixTm ++ "+" ++ show secPerMv
otherOpts = "-pgnin swcr-20.pgn -recover -draw 150 100 -resign 4 800 -site Sixpack"
expLength :: Int
expLength = 200	-- expect a maximum game length of so may moves
-- how long to wait for a match - in milliseconds
toFact  = 2
expDur :: Double
expDur  = fromIntegral expLength * secPerMv * 1000
              + fromIntegral ((expLength * 1000 `div` tcMoves) * tcFixTm)
timeOut :: Int
timeOut = toFact * noGames * round expDur
ecom  = engPath ++ "\\" ++ engine ++ ".exe"
-- outDir    = "J:\\AbaAba\\Tests\\Self"
-- res   = "result_" ++ conf1 ++ "_" ++ conf2 ++ ".pgn"
resp  = "-pgnout "

engSpec name = "-engine cmd=" ++ ecom ++ " name=" ++ name ++ " arg=" ++ name

each dir = "-each dir=" ++ dir ++ " proto=uci " ++ parTime

-- Parameters for optimisation
popCount = 12	-- population count (without witness)
qualiFrc = 50	-- percent of the population which qualifies for next tournament
eliteFrc = 50	-- percent of the qualifiers which is elite (determine the new distribution)
distStep = 0.5	-- distribution changing step

-- Parameters for variation of the random generated candidates
unchangedChances = 8 :: Int	-- remain unchanged in 8 from 8+2 cases

-- The global optimisation method used here is an adapted cross entropy method
-- where the new samples are added to the best samples of the previous step
-- This makes sense only when the comparison of the samples is not deterministic
-- which is the case when comparing them by playing tournaments (the results
-- of the games are not deterministic)

-- To start a new evolve give directory and a name
-- To continue an interrupted evolve, just give the directory (default: pwd)
main = do
    (proc : seed : params)  <- getArgs
    let dict = makeDict params
    res <- runGame dict
    putStrLn res

-- Take a list of param/values sequence (as strings) and structure it
-- in form [(param, value)]
makeDict :: [String] -> [(String, Double)]
makeDict ss = go Nothing ss []
    where go Nothing [] acc = acc
          go _       [] _   = error "Unmatched param/value sequence"
          go Nothing  (p:ps) acc = go (Just p) ps acc
          go (Just p) (v:ps) acc = go Nothing  ps ((p, read v):acc)

runGame :: [(String, Double)] -> IO String
runGame = runFakeGame

-- This is for test of the driver (and the QLR method)
-- and is generating a random result with some probability distribution
runFakeGame :: [(String, Double)] -> IO String
runFakeGame ((_,v):_) = do
    -- The target value of the parameter is 10
    let dist  = abs (v - 10)
        decay = 0.1
        estim = max 0 $ 0.5 - dist * decay
    r <- getStdRandom (randomR (0, 1)) :: IO Double
    if r < estim
       then return "W"
       else return "L"

-- Give names to the candidates
nameCandidates evn cycle = zip (map label [1..])
    where label i = evn ++ "-" ++ show cycle ++ "-" ++ show i ++ ".txt"

-- writeCandidates cs = do
--     ctime <- getClockTime >>= toCalendarTime >>= return . calendarTimeToString
--     mapM_ (writeCandidate ctime) cs

-- Write the candidate configuration to a file
-- writeCandidate ctime (name, vec) = writeFile (currentDir </> name) (showConfig vec comm)
--     where comm = "-- Candidate " ++ name ++ " created on " ++ ctime

eventName :: Event -> String
eventName = id

-- eventDir :: Event -> String
-- eventDir = (outDir </>) . eventName

type Event = String
type Player = String

oneMatch :: Event -> Bool -> Player -> Player -> IO (Int, Int, Int)
oneMatch event pgn p1 p2 = do
    cdir <- getCurrentDirectory
    let edir = cdir </> currentDir
        evname = eventName event
        pfname = evname ++ ".pgn"
        pfil = cdir </> gamesDir </> pfname
        args1 = concatMap words [
                  parGames,
                  otherOpts,
                  "-event", evname,
                  each edir,
                  engSpec p1,
                  engSpec p2
               ]
        args2 = concatMap words [ resp, pfil ]
        args = if pgn then args1 ++ args2 else args1
    -- when debug $ putStrLn $ "Start: " ++ unwords (cuteChessCom : args)
    (_, Just hout, _, ph)
            <- createProcess (proc cuteChessCom args) { std_out = CreatePipe, cwd = Just cuteChessDir }
    catch (everyLine hout (0, 0, 0) noGames) $ \e -> do
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

pairings :: [a] -> [(a,a)]
pairings [] = []
pairings (a:as) = zip (repeat a) as ++ pairings as

alternate :: Bool -> [(a,a)] -> [(a,a)]
alternate _ [] = []
alternate False (xy : xys) = xy : alternate True xys
alternate True ((x, y) : xys) = (y, x) : alternate False xys

uniform :: IO Double
uniform = getStdRandom (randomR (-1, 1))

variate :: Double -> IO Double
variate x = do
    r <- getStdRandom (randomR (-1, unchangedChances))
    case r of
        -1 -> return $ x - 1
        0  -> return $ x + 1
        _  -> return x

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
