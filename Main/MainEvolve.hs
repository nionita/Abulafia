{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import Prelude hiding (catch)
import Control.Concurrent
import Control.Concurrent.Async
-- import Control.Concurrent.MVar
-- import Control.Concurrent.Chan
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

import Struct.Status
import Struct.Evolve
import Eval.Eval

debug = False

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
    args  <- getArgs
    evDir <- if null args then getCurrentDirectory else return (head args)
    -- kern <- getNumCapabilities
    let evName = if length args > 1 then Just (args!!1) else Nothing
        -- mxt = kern `div` 2	-- start in the middle
        mxt = 3
    putStrLn $ "Threads: " ++ show mxt
    pstate <- initState evDir evName mxt
    runStateT evolveOrStop EvSt { stPers = pstate, stAsync = M.empty,
                                  stMaxThr = mxt, stCurThr = 0 }

initState :: FilePath -> Maybe String -> Int -> IO EvolvePersistentState
initState dir mname mxt = do
    st <- case mname of
          Just evname -> initNewState dir evname
          Nothing     -> initOldState dir
    writeFile goonFile $ show mxt
    return st

-- Initialize a new evolve, return state
initNewState :: FilePath -> String -> IO EvolvePersistentState
initNewState dir evname = do
    putStrLn $ "Initialize new evolve " ++ evname ++ " in " ++ dir
    createDirectory dir
    createDirectory $ dir </> playersDir
    createDirectory $ dir </> gamesDir
    createDirectory $ dir </> currentDir
    setCurrentDirectory dir
    let evs = Pers { evName = evname, evPopCount = popCount, evCycle = 0, evPhase = Initialize,
                     evDistrib = initDist, evPParams = [], evCurTour = Nothing,
                     evWitness = Nothing, evWitSucc = [], evActSucc = []
                   }
    saveState evs
    return evs

-- Initialize an old evolve (status from file)
initOldState :: FilePath -> IO EvolvePersistentState
initOldState dir = do
    setCurrentDirectory dir
    readFile statusFile >>= \s -> return (correctTour $ read s)

-- When reading the persistent state from file, all the games which were
-- "playing" must be transformed in "to play", because at that moment
-- no thread is running
correctTour :: EvolvePersistentState -> EvolvePersistentState
correctTour eps = case evCurTour eps of
    Nothing  -> eps
    Just trn -> eps { evCurTour = Just $ noPlaying trn }
    where noPlaying t = t { games = map f (games t) }
          f (Pairing p Playing) = Pairing p ToPlay
          f x                   = x

-- We control the number of threads as a number written in the "running" file
howManyThreads :: IO Int
howManyThreads = do
    ifc <- threadsFromFile `catch` retZero	-- should check what kind of exception
    -- kern <- getNumCapabilities
    -- return $ max 0 $ min ifc kern	-- give some reasonable limits
    return $ max 0 $ min ifc 6	-- give some reasonable limits
    where retZero :: SomeException -> IO Int
          retZero = \_ -> return 0

threadsFromFile :: IO Int
threadsFromFile = readFile goonFile >>= return . read . head . lines

-- Generate an initial distribution, which gaussian in every component
-- (parameter to optimize) and has as the mean the default parameters from Eval.hs
-- and as standard deviation the maximum (absolute value) of each parameter
initDist = (parDim, (def, vars0))
    where ies = initEvalState []
          def = esDParams ies
          vars0 = map f parLims
          f (mi, ma) = max (abs mi) (abs ma)

getPersState = gets stPers
putPersState p = modify $ \s -> s { stPers = p }

-- The gracefully exit is by deleting the witness file
-- (which is named "running"), after which at the next loop
-- the program will terminate
-- To change the number of threads, echo the new number to the file:
-- echo 2 > running
-- It takes some time (seconds to minutes) until the reaction to
-- these changes is visible
evolveOrStop = do
    goon <- lift $ howManyThreads
    evst <- get
    when (goon /= stMaxThr evst) $ lift
        $ putStrLn $ "Max threads set to " ++ show goon
                   ++ ", currently " ++ show (stCurThr evst) ++ " threads running"
    if goon > 0 || stCurThr evst > 0
       then evolveState goon >> gets stPers >>= \s -> lift (saveState s) >> evolveOrStop
       else lift $ putStrLn "Exiting"

evolveState goon = do
    phase <- gets $ evPhase . stPers
    case phase of
        Initialize -> stateInitial
        Prepare    -> statePrepare
        Play       -> statePlay goon

stateInitial = do
    est <- getPersState
    lift $ putStrLn $ "Initialising new evolve " ++ evName est
    newVecs <- lift $ genCandidates (evDistrib est) (evPopCount est)
    let cycle  = evCycle est + 1
        candps = nameCandidates (evName est) cycle newVecs
        cands  = map fst candps
        succ   = zip cands $ repeat 0
        tour   = makeTournament (evName est ++ "-" ++ show cycle) cands
    lift $ writeCandidates candps
    putPersState est {
           evPhase = Play, evCycle = cycle, evPParams = candps, evCurTour = Just tour, evActSucc = succ
        }

statePrepare = do
    est <- getPersState
    lift $ putStrLn $ "Preparing new run for evolve " ++ evName est
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
    lift $ do
        writeTourTop (event ltour ++ "-top.txt") selecs
        writeTourRes (event ltour ++ "-res.txt") (games ltour) (players ltour)
        putStr "Elite:"
        forM_ elite $ \e -> putStr (" " ++ e)
        putStrLn ""
    newVecs <- lift $ genCandidates dist missing
    let ncandps = nameCandidates (evName est) cycle newVecs
        ncands  = map fst ncandps
        nsucc   = zip ncands $ repeat 0
        tour    = makeTournament (evName est ++ "-" ++ show cycle) $ ncands ++ good
    -- move all weak players to the player directory
    lift $ do
        forM_ weak $ \p -> renameFile (currentDir </> p) (playersDir </> p)
        writeCandidates ncandps
    putPersState est {
           evPhase = Play, evDistrib = dist, evCycle = cycle, evPParams = pars ++ ncandps,
           evCurTour = Just tour, evActSucc = succ ++ nsucc
        }

getVect :: [(Player, Vec)] -> Player -> Maybe Vec
getVect = flip lookup

getQuota :: Int -> Int -> Int
getQuota q k = max 2 $ (k * q + 50) `div` 100

statePlay goon = do
    currt <- gets stMaxThr
    when (goon /= currt) $ modify $ \s -> s { stMaxThr = goon }
    est <- getPersState
    (done, mtrn) <- tourStep $ fromJust $ evCurTour est
    if done then putPersState est { evPhase = Prepare }
            else case mtrn of
                Just _ -> putPersState est { evCurTour = mtrn }
                _      -> return ()

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

-- Saving the status file to disk in order to recover, if necessary
-- To be more robust, write to a new file and then renaming
saveState st = do
    writeFile statusFileN $ show st
    renameFile statusFileN statusFile

writeTourTop fil evalt = do
    let info = unlines
             $ map (\(i, (pl, (pt, pg)))
                      -> printf "%2d. %-30s %5.1g %7.1g %5.1g" i pl (f pt) (f pg) (p pt))
             $ zip places evalt
    writeFile (gamesDir </> fil) info
    where places = [1..] :: [Int]
          f :: Rational -> Double
          -- f r = fromIntegral (numerator r) / fromIntegral (denominator r)
          f r = fromRational r
          p :: Rational -> Double
          p r = fromRational r * 100 / maxp
          maxp = fromIntegral $ noGames * (popCount - 1)

writeTourRes fil gs ps = do
    let info = unlines
             $ map (\(a, b, w, l, d)
                      -> printf "%-50s %2d wone, %2d lost, %2d draw" (a ++ " - " ++ b) w l d)
             $ map toTuple
             $ filter gameDone	-- just to be sure...
             $ sortBy (comparing pair) gs
    writeFile (gamesDir </> fil) info
    where toTuple (Pairing (a, b) (Done (w, l, d))) = (ps!!a, ps!!b, w, l, d)
          gameDone (Pairing _ (Done _)) = True
          gameDone _                    = False

eventName :: Event -> String
eventName = id

-- eventDir :: Event -> String
-- eventDir = (outDir </>) . eventName

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
    when debug $ putStrLn $ "Start: " ++ unwords (cuteChessCom : args)
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
    when debug $ putStrLn $ "Got: " ++ lin
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

makeTournament :: Event -> [Player] -> Tournament
makeTournament ev ps = Tournament { event = ev, players = ps, games = prs }
    where prs = map (\xy -> Pairing xy ToPlay) $ alternate False $ pairings [0..n-1]
          n = length ps

categorize :: [Pairing] -> ([Pairing], [Pairing], [Pairing])
categorize = go ([], [], [])
    where go a [] = a
          go (d, p, w) (g:gs)
              | ToPlay  <- result g = go (d, p, g:w) gs
              | Playing <- result g = go (d, g:p, w) gs
              | otherwise           = go (g:d, p, w) gs

findGame :: Int -> Int -> [Pairing] -> Maybe (Pairing, [Pairing])
findGame i j = go []
    where go as [] = Nothing
          go as (g@(Pairing (k, l) _) : gs)
              | k == i && l == j = Just (g, as ++ gs)
              | otherwise        = go (g:as) gs

-- Exception to throw when an unexpected pairing finishes
data GameNotFoundException = GameNotFoundException deriving (Show, Typeable)
instance Exception GameNotFoundException

tourStep :: Tournament -> StateT EvolveState IO (Bool, Maybe Tournament)
tourStep trn
    | null playing && null waiting = return (True, Nothing)
    | otherwise = do
        evst <- get
        -- Start as many games as alowed
        let tostart = stMaxThr evst - stCurThr evst
        -- lift $ putStrLn $ "Max " ++ show (stMaxThr evst) ++ " crt " ++ show (stCurThr evst)
        --                     ++ " to start " ++ show tostart
        asyncs' <- lift $ startNewGames (event trn) tostart waiting (players trn)
        let (playing'', waiting') = splitAt tostart waiting
            playing' = map nowPlaying playing''
            trn' = trn { games = done ++ playing ++ waiting' ++ playing' }
            started = length playing'
            asyncs = stAsync evst `M.union` asyncs'
        -- lift $ putStrLn $ "Playing list: " ++ show (playing ++ playing')
        -- wait for one of the games to finish
        (a, eir) <- lift $ waitAnyCatch $ M.keys asyncs
        let mij = M.lookup a asyncs	-- which game finnished?
        case mij of
            Nothing -> do
                lift $ putStrLn $ "Async not found in map, ignored"
                put evst { stCurThr = stCurThr evst + started,
                           stAsync = M.delete a asyncs }
                return (False, Just trn')
            Just (i, j) -> do
                let pl1 = players trn !! i
                    pl2 = players trn !! j
                    mfg = findGame i j $ playing ++ playing'
                -- lift $ putStrLn $ "Received " ++ show i ++ " / " ++ show j
                case mfg of
                    Nothing -> lift $ do
                        putStrLn $ "Pair " ++ show i ++ ", " ++ show j ++ " not playing"
                        throwIO GameNotFoundException
                    Just (game, stillplaying) -> do
                        -- lift $ putStrLn $ "Game: " ++ show game
                        -- lift $ putStrLn $ "Still: " ++ show stillplaying
                        let rest = done ++ stillplaying ++ waiting'
                        ngame <- case eir of
                            Left  e    -> do
                               lift $ putStrLn $ "async ended with exception: " ++ show e
                               return game { result = ToPlay }
                            Right mrez -> do
                               lift $ putStrLn $ "Match " ++ pl1 ++ " against " ++ pl2
                                          ++ " ended: " ++ show mrez
                               case mrez of
                                   Nothing -> return game { result = ToPlay }
                                   Just rz -> return game { result = Done rz }
                        put evst { stCurThr = stCurThr evst + started - 1,
                                   stAsync = M.delete a asyncs }
                        return (False, Just trn' { games = ngame : rest })
    where (done, playing, waiting) = categorize $ games trn
          nowPlaying g = g { result = Playing }

startNewGames :: Event -> Int -> [Pairing] -> [Player]
              -> IO (M.Map (Async ResReturn) (Int, Int))
startNewGames ev n ps pls = do
    as <- forM (take n ps) $ \(Pairing (i, j) _) -> do
        let pl1 = pls !! i
            pl2 = pls !! j
            pgn = i == 6 || j == 6
        stime <- getClockTime >>= toCalendarTime
                              >>= return . formatCalendarTime defaultTimeLocale "%H:%M:%S"
        putStrLn $ stime ++ ": playing " ++ pl1 ++ " against " ++ pl2
        -- The timeout is in microseconds, which is bad for longer matches,
        -- because we reach the Int max bound! So we trust on cutechess_cli
        -- that it will not hang...
        -- a <- async $ (timeout to $ oneMatch ev pgn pl1 pl2) `catch` retNothing
        a <- async $ (oneMatch ev pgn pl1 pl2 >>= return . Just) `catch` retNothing
        return (a, (i, j))
    return $ M.fromList as
    where to = timeOut*1000	-- here time is in micro seconds
          retNothing :: SomeException -> IO (Maybe a)
          retNothing = \_ -> return Nothing

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

toPoints (Pairing { pair = (i, j), result = Done (w, l, r) })
    = [(i, 2*w + r), (j, 2*l + r)]
toPoints _ = []

showConfig cnf comm = comm ++ "\n" ++ lins
    where lins = unlines $ map (\(n, v) -> n ++ " = " ++ show v) $ zip paramNames cnf

-- Generate new candidates from the distribution
genCandidates :: Distrib -> Int -> IO [Vec]
genCandidates dist n = forM [1..n] $ \_ -> genOneCand dist

-- Generating one candidate given the means and variations of the parameters
-- We must generate gaussian random numbers around the means with the corresponding sigma
-- and then bring them in limits (a simple minimum/maximum operation)
-- We then round each value (as the actual parameters will be the integer ones)
-- and random variate some of them with +1 or -1 (with low probability)
genOneCand (_, (means, vars)) = do
    pars  <- mapM (uncurry fitNormal) $ zip means $ map sqrt vars
    pars' <- mapM variate pars
    return $ map rounding . inLimits parLims $ pars'
    where rounding x = let y = round x :: Int in fromIntegral y

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
