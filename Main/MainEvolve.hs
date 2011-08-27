module Main (main) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (forM, forM_)
import Data.List (intersperse, isPrefixOf, sortBy, groupBy)
import Data.Maybe
import Data.Ord (comparing)
import Data.Ratio
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.IO
import System.IO.Error
import System.Process
import System.Time
import System.Locale (defaultTimeLocale)
import System.Random
import Text.Printf

import Struct.Status
import Struct.Evolve
import Eval.Eval

-- Some constants for the evolving environment
playersDir = "Players"
gamesDir   = "Games"
currentDir = "Current"
statusFile = "status.txt"
goonFile   = "running"

-- Some constants for playing one match
cuteChess = "J:\\Chess\\cutechess-cli-win32\\cutechess-cli.exe"
engPath   = "J:\\AbaAba\\dist\\build\\AbaAba"
engine    = "AbaAba_0_58_pal1"
noGames   = 2
parGames  = "-games " ++ show noGames
tcMoves   = 20	-- moves
tcFixTm   = 10	-- seconds
secPerMv  = 0	-- second per move
parTime   = "tc=" ++ show tcMoves ++ "/" ++ show tcFixTm ++ "+" ++ show secPerMv
otherOpts = "-resign 4 800 -site Sixpack"
expLength = 200	-- expect a maximum game length of so may moves
-- how long to wait for a match
timeOut   = noGames * 2
          * (expLength * secPerMv + ((expLength + tcMoves) `div` tcMoves) * tcFixTm)
ecom  = engPath ++ "\\" ++ engine ++ ".exe"
-- outDir    = "J:\\AbaAba\\Tests\\Self"
-- res   = "result_" ++ conf1 ++ "_" ++ conf2 ++ ".pgn"
resp  = "-pgnout "

peng name first = pos ++ "cp cmd=" ++ ecom ++ " name=" ++ name ++ " arg=" ++ name
    where pos = if first then "-f" else "-s"
firstEngine  name = peng name True
secondEngine name = peng name False

both dir = "-both dir=" ++ dir ++ " proto=uci " ++ parTime

-- Parameters for optimisation
popCount = 12	-- population count (without witness)
qualiFrc = 50	-- percent of the population which qualifies for next tournament
eliteFrc = 50	-- percent of the qualifiers which is elite (determine the new distribution)
distStep = 0.5	-- distribution changing step

main = mainEvolve

mainReal = do
    putStrLn $ "Match timeout: " ++ show timeOut ++ " seconds"
    let trn = makeTournament "Event1" pls
        -- pls = ["ep-1.txt", "ep-2.txt", "ep-3.txt", "ep-4.txt", "ep-5.txt"]
        pls = take 10 $ repeat "ep-5.txt"
    trn' <- runTournament trn 3
    putStrLn $ "End:"
    putStrLn $ show trn'
    putStrLn "Final table:"
    forM_ (evalTournament trn') $ \(p, s) -> putStrLn (p ++ "\t\t" ++ show s)

-- To start a new evolve give directory and a name
-- To continue an interrupted evolve, just give the directory (default: pwd)
mainEvolve = do
    args  <- getArgs
    evDir <- if null args then getCurrentDirectory else return (head args)
    let evName = if length args > 1 then Just (args!!1) else Nothing
    state <- initState evDir evName
    evolveOrStop state

initState :: FilePath -> Maybe String -> IO EvolveStatus
initState dir mname = do
    st <- case mname of
          Just evname -> initNewState dir evname
          Nothing     -> initOldState dir
    writeFile goonFile ""
    return st

-- Initialize a new evolve, return state
initNewState :: FilePath -> String -> IO EvolveStatus
initNewState dir evname = do
    putStrLn $ "Initialize new evolve " ++ evname ++ " in " ++ dir
    createDirectory dir
    createDirectory $ dir </> playersDir
    createDirectory $ dir </> gamesDir
    createDirectory $ dir </> currentDir
    setCurrentDirectory dir
    let evs = Evs { evName = evname, evPopCount = popCount, evCycle = 0, evPhase = Initialize,
                    evDistrib = initDist, evPParams = [], evCurTour = Nothing,
                    evWitness = Nothing, evWitSucc = [], evActSucc = []
                  }
    writeFile statusFile $ show evs
    return evs

-- Initialize an old evolve (status from file)
initOldState :: FilePath -> IO EvolveStatus
initOldState dir = do
    setCurrentDirectory dir
    readFile statusFile >>= \s -> return (read s)

initDist = (parDim, (def, vars0))
    where ies = initEvalState False []
          def = esDParams ies
          vars0 = map f parLims
          f (mi, ma) = max (abs mi) (abs ma)

evolveOrStop est = do
    goon <- doesFileExist goonFile
    if goon
       then do
           nst <- evolveState est
           writeFile statusFile $ show nst
           evolveOrStop nst
       else putStrLn "Running file not found, exiting"

evolveState est = case evPhase est of
    Initialize -> stateInitial est
    Prepare    -> statePrepare est
    Play       -> statePlay    est

stateInitial est = do
    putStrLn $ "Initialising new evolve " ++ evName est
    newVecs <- genCandidates (evDistrib est) (evPopCount est)
    let cycle  = evCycle est + 1
        candps = nameCandidates (evName est) cycle newVecs
        cands  = map fst candps
        succ   = zip cands $ repeat 0
        tour   = makeTournament (evName est ++ "-" ++ show cycle) cands
    writeCandidates candps
    return est {
           evPhase = Play, evCycle = cycle, evPParams = candps, evCurTour = Just tour, evActSucc = succ
        }

statePrepare est = do
    putStrLn $ "Preparing new run for evolve " ++ evName est
    let cycle = evCycle est + 1
        ltour = fromJust $ evCurTour est
        evalt = evalTournament ltour
        selecs = reverse . sortBy (comparing snd)
                     $ makeSelectionBase (evWitness est) evalt (evActSucc est)
        succ = map (\(p, (s1, s2)) -> (p, s1 + s2)) selecs
        qquota = getQuota qualiFrc (evPopCount est)
        equota = getQuota eliteFrc qquota
        (good, weak)  = splitAt qquota $ map fst succ
        elite = take equota good
        pars  = filter (not . flip elem weak . fst) (evPParams est)
        dist  = newDist (evDistrib est) $ mapMaybe (getVect pars) elite
        missing = evPopCount est - length weak
    writeTourInfo (event ltour ++ ".txt") selecs
    putStr "Elite:"
    forM_ elite $ \e -> putStr (" " ++ e)
    putStrLn ""
    newVecs <- genCandidates dist missing
    let ncandps = nameCandidates (evName est) cycle newVecs
        ncands  = map fst ncandps
        nsucc   = zip ncands $ repeat 0
        tour    = makeTournament (evName est ++ "-" ++ show cycle) $ ncands ++ good
    -- move all weak players to the player directory
    forM_ weak $ \p -> renameFile (currentDir </> p) (playersDir </> p)
    writeCandidates ncandps
    return est {
           evPhase = Play, evDistrib = dist, evCycle = cycle, evPParams = pars ++ ncandps,
           evCurTour = Just tour, evActSucc = succ ++ nsucc
        }

getVect :: [(Player, Vec)] -> Player -> Maybe Vec
getVect = flip lookup

getQuota :: Int -> Int -> Int
getQuota q k = max 2 $ (k * q + 50) `div` 100

statePlay est = do
    (done, mtrn) <- tourStep $ fromJust $ evCurTour est
    if done then return est { evPhase = Prepare }
            else case mtrn of
                Just _ -> return est { evCurTour = mtrn }
                _      -> return est

-- Give names to the candidates
nameCandidates evn cycle = zip (map label [1..])
    where label i = evn ++ "-" ++ show cycle ++ "-" ++ show i ++ ".txt"

writeCandidates cs = do
    ctime <- getClockTime >>= toCalendarTime >>= return . calendarTimeToString
    mapM_ (writeCandidate ctime) cs

-- Write the candidate configuration to a file
writeCandidate ctime (name, vec) = writeFile (currentDir </> name) (showConfig vec comm)
    where comm = "-- Candidate " ++ name ++ " created on " ++ ctime

-- Consider the older successes of the players and eventually remove the witness
makeSelectionBase :: Maybe Player -> [(Player, Rational)] -> [(Player, Rational)]
                  -> [(Player, (Rational, Rational))]
makeSelectionBase mwit ordl olds = remWitn $ map addOlds ordl
    where addOlds (p, s) = case lookup p olds of Just os -> (p, (s, os)); Nothing -> (p, (s, 0))
          remWitn sbs = case mwit of Just p -> filter ((/= p) . fst) sbs; Nothing -> sbs

writeTourInfo fil evalt = do
    let info = unlines
             $ map (\(i, (p, (pt, pg))) -> printf "%2d. %-30s %4.1g %6.1g" i p (f pt) (f pg))
             $ zip places evalt
    writeFile (gamesDir </> fil) info
    where places = [1..] :: [Int]
          f :: Rational -> Double
          f r = fromIntegral (numerator r) / fromIntegral (denominator r)

eventName :: Event -> String
eventName = id

-- eventDir :: Event -> String
-- eventDir = (outDir </>) . eventName

oneMatch :: Event -> Bool -> Player -> Player -> IO Result
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
                  both edir,
                  firstEngine p1,
                  secondEngine p2
               ]
        args2 = concatMap words [ resp, pfil ]
        args = if pgn then args1 ++ args2 else args1
    (_, Just hout, _, ph)
            <- createProcess (proc cuteChess args) { std_out = CreatePipe }
    catch (everyLine hout (0, 0, 0) noGames) $ \e -> do
        let es = ioeGetErrorString e
        putStrLn $ "Error in everyLine: " ++ es
        terminateProcess ph
        return Nothing

everyLine _ r 0 = return $ Just r
everyLine h r g = do
    lin <- hGetLine h
    let (r1, g1) = if "Score of" `isPrefixOf` lin
                      then (getScore lin, g-1)
                      else (r, g)
    everyLine h r1 g1

getScore :: String -> (Int, Int, Int)
getScore
    = listToTrio
    . map (read . snd)
    . filter (even . fst)
    . zip [0..]
    . reverse
    . take 5
    . reverse
    . words

listToTrio (x:y:z:_) = (x, y, z)

pairings [] = []
pairings (a:as) = zip (repeat a) as ++ pairings as

alternate _ [] = []
alternate False (xy : xys) = xy : alternate True xys
alternate True ((x, y) : xys) = (y, x) : alternate False xys

makeTournament :: Event -> [Player] -> Tournament
makeTournament ev ps = Tournament { event = ev, players = ps, games = prs }
    where prs = map (\xy -> Pairing xy Nothing) $ alternate False $ pairings [0..n-1]
          n = length ps

tourDone :: Tournament -> Bool
tourDone = null . filter (isNothing . result) . games

tourStep :: Tournament -> IO (Bool, Maybe Tournament)
tourStep trn = if tourDone trn then return (True, Nothing) else do
    let (played, unplayed) = span (isJust . result) $ games trn
    let game = head unplayed
        (i, j) = pair game
        rest = tail unplayed
        pl1 = players trn !! i
        pl2 = players trn !! j
        pgn = i == 6 || j == 6
    stime <- getClockTime >>= toCalendarTime
                          >>= return . formatCalendarTime defaultTimeLocale "%H:%M:%S"
    putStrLn $ stime ++ ": playing " ++ pl1 ++ " against " ++ pl2
    -- irand <- randomRIO (0, 9)	-- one of 10 games written to pgn
    rez <- withTimeOut (oneMatch (event trn) pgn pl1 pl2) (timeOut*1000*1000) Nothing
    putStrLn $ "Match " ++ pl1 ++ " against " ++ pl2 ++ " ended: " ++ show rez
    case rez of
        Just rz -> return (False, Just trn { games = game { result = Just rz } : played ++ rest })
        _       -> return (False, Nothing)

runTournament :: Tournament -> Int -> IO Tournament
runTournament trn 0 = return trn
runTournament trn n = do
    (done, mtrn) <- tourStep trn
    if done then return trn else case mtrn of
            Just trn' -> runTournament trn' n
            _         -> runTournament trn (n-1)

evalTournament :: Tournament -> [(Player, Rational)]
evalTournament trn
    = map inReal
    . reverse
    . sortBy (comparing snd)
    . map fact
    . groupBy samePlayer
    . sortBy (comparing fst)
    . concatMap toPoints $ games trn
    where samePlayer (i, _) (j, _) = i == j
          fact as = (fst $ head as, sum $ map snd as)
          inReal (i, s) = (players trn !! i, fromIntegral s / 2)

toPoints pr | result pr == Nothing = []
toPoints (Pairing { pair = (i, j), result = Just (w, l, r) })
    = [(i, 2*w + r), (j, 2*l + r)]

-- Run an IO action with timeout (in microseconds) and default value
withTimeOut :: IO a -> Int -> a -> IO a
withTimeOut act to def = do
    mvar <- newEmptyMVar
    longtid  <- forkIO (act >>= putMVar mvar)
    sleeptid <- forkIO (threadDelay to >> putMVar mvar def)
    rv <- takeMVar mvar
    forkIO (killThread sleeptid >> killThread longtid)
    return rv

showConfig cnf comm = comm ++ "\n" ++ lins
    where lins = unlines $ map (\(n, v) -> n ++ " = " ++ show v) $ zip paramNames cnf

genCandidates :: Distrib -> Int -> IO [Vec]
genCandidates dist n = forM [1..n] $ \_ -> genOneCand dist

genOneCand (_, (means, vars))
    = fmap (inLimits parLims) $ mapM (uncurry fitNormal) $ zip means $ map sqrt vars

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

-- Calculate the new distribution based on the elite samples
newDist :: Distrib -> [Vec] -> Distrib
newDist (ddim, (omeans, ovars)) vs = (ddim, (nmeans, nvars))
    where means = map ( / fln) $ foldr (zipWith (+)) zero vs
          vars  = map ( / fln1) $ foldr (zipWith (+) . zipWith f means) zero vs
          f x y = let d = x - y in d * d
          nmeans = moveTo omeans means
          nvars  = moveTo ovars  vars
          fln  = fromIntegral $ length vs
          fln1 = fln - 1
          zero = replicate ddim 0

moveTo old new = zipWith (+) (map (* ost) old) (map (* distStep) new)
    where ost = 1 - distStep

{--
mainTest = withTimeOut getPowerInput 3000000 ()

getPowerInput = do
    let power = "powershell.exe"
        args = [
               "sleep", "2", ";",
               "get-process", "cmd", ";",
               "sleep", "5", ";",
               "get-process", "svchost"
               ]
    (_, Just hout, _, ph)
            <- createProcess (proc power args) { std_out = CreatePipe }
    -- putStrLn "Created process, sleeping 2 sec"
    -- threadDelay (2000*1000)	-- sleep one second to give the process time to set up
    -- putStrLn "Begin to read the process output"
    catch (powerLine hout) $ \e -> do
        putStrLn $ "Error: " ++ show e
        terminateProcess ph

powerLine h = do
    eof <- hIsEOF h
    if eof then return () else do
        -- intime <- hWaitForInput h waitMilli
        -- if not intime then return Nothing else do
            -- putStrLn $ "intime = " ++ show intime
            lin <- hGetLine h
            putStrLn $ "--> " ++ lin
            powerLine h
--}
