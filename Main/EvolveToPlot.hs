module Main where

import Control.Applicative
import Control.Monad (forM_, mapM)
import qualified Data.Map as Map
import Data.Maybe (isJust, catMaybes)
import Data.List (group, sort, sortBy)
import Data.Ord (comparing)
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import System.IO
import Text.Regex

import Struct.Status
import Eval.Eval
import Eval.FileParams

regexResultFile = mkRegex "(.*-([0-9]+)\\.txt)"
regexPlayerName = mkRegex " *[0-9]+\\. ([^ ]+)"

main = mainToPlot

mainToPlot = do
    args <- getArgs
    case args of
        (dir : places : _) -> toPlot dir (read places)
        (dir : _)          -> toPlot dir 3
        _ -> putStrLn "Run with directory and optional with places as parameters!"

toPlot dir places = do
    let gamesDir = dir </> "Games"
        currDir  = dir </> "Current"
        archDir  = dir </> "Players"
    putStrLn $ "Get the games from " ++ gamesDir
    rfiles <- getResultFiles gamesDir
    let sfiles = sortBy (comparing fst) $ map (\(fil:num:_) -> (read num :: Int, fil)) rfiles
    putStrLn $ show (length rfiles) ++ " games found"
    putStrLn $ "Read the games results (first " ++ show places ++ " places)"
    pla <- mapM (\(_, fil) -> getWinPlayers (gamesDir </> fil) places) sfiles
    let winning = zip (map fst sfiles) pla	-- [(tourn, [pla1, pla2, pla3])]
        dist    = map head . group . sort . concat $ pla
    putStrLn $ show (length dist) ++ " distinct players found"
    putStrLn "Getting the players patameters"
    plMap <- Map.fromList . map (\(p, Just d) -> (p, d)) . filter (isJust . snd) . zip dist
                 <$> mapM (getPlayer currDir archDir) dist
    putStrLn "Compute the averages"
    let winave = map (calcAverage plMap) winning
        txt    = unlines $ map (\(t, ps) -> show t ++ " " ++ unwords (map show ps)) winave
    -- forM_ winave $ \(t, ps) -> putStrLn $ show t ++ " --> " ++ show ps
    writeFile "results.txt" $ "# " ++ (unwords paramNames) ++ "\n" ++ txt
    return ()


getResultFiles dir = do
    files <- getDirectoryContents dir
    let rfiles = catMaybes $ map (matchRegex regexResultFile) files
    return rfiles

getWinPlayers fil places = do
    rcon <- readFile fil
    return $ map head $ catMaybes $ map (matchRegex regexPlayerName) $ take places $ lines rcon

getPlayer d1 d2 fil = do
    let f1 = d1 </> fil
        f2 = d2 </> fil
    cex <- doesFileExist f1
    if cex then Just . esDParams <$> fileToState False f1
           else do
               pex <- doesFileExist f2
               if pex then Just . esDParams <$> fileToState False f2
                      else return Nothing

calcAverage pmp (t, pls) = (t, map (/n) $ foldl (zipWith (+)) (repeat 0) vs)
    where vs = catMaybes $ map (flip Map.lookup pmp) pls
          n  = fromIntegral $ length vs
