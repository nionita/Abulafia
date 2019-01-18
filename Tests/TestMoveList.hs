module Main (main) where

import Control.Monad
import Control.Monad.State (evalState)
import Data.Char (isSpace)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
-- import System.Time
import Criterion.Main

import Struct.Struct
import Struct.Status
import Moves.Moves
import Moves.Board
import Moves.Base
import Moves.History
import Eval.Eval
import Hash.TransTab
import Search.Albeta
import Search.AlbetaTypes
import Search.SearchMonad
import Config.ConfigClass
import Config.Config
import Tests.DummyCtxMonIO

-- Simple test program for the move list:
-- given a option A/C and a fen on command line (arguments)
-- list the moves (all or only captures, as in quiescent search)
-- on the output

main = do
    args <- getArgs
    if length args < 2
       then putStrLn "Start it with option (A/C) and fen!"
       else do
           let capts = head args == "C"
               fen   = unwords $ tail args
           putStrLn $ (if capts then "Captures" else "Moves") ++ " for fen: " ++ fen
           st <- initState fen
           let act = if capts then genTactMoves else genMoves 0 0 True Nothing []
           (mvs, _) <- runSearch act st
           putStrLn "After move generation..."
           -- forM_ mvs $ \m -> putStr " " >> putStr (show m)
           forM_ mvs $ \m -> putStrLn (show m)
           putStrLn $ "\nNumber of moves: " ++ show (length mvs)

-- Creates a state from the given fen string
initState fen = do
    let pos = updatePos $ posFromFen fen
    ha  <- newCache defaultConfig
    hi  <- newHist
    evs <- makeEvalState Nothing
    return $ posToState pos ha hi evs

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState argfile =
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

-- tellInIO :: Comm Move Int -> IO ()
-- tellInIO _ = return ()
