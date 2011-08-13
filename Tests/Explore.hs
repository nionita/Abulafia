{-# LANGUAGE GADTs #-}

module Main where

import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.MVar (readMVar)
import System.IO (stdout, hFlush)
import System.Exit

import Struct.Struct
import Struct.Context
import Struct.Status
import Moves.Base
import Moves.Board
import Hash.SimpleCache
import Moves.History
import Uci.UCI
import Uci.UciMain (initContext)
import Search.SearchMonad (runSearch)
import Config.ConfigClass
import Config.Config

main = do
    ctx <- initContext (GConfig defaultConfig)
    tt <- readMVar (change ctx) >>= return . hash . crtStatus
    hi <- liftIO newHist
    runReaderT (runSearch explore $ posToState initPos tt hi) ctx

explore :: Game ()
explore = prompt >> readdecode >>= interpret >> explore

out = liftIO . putStrLn
prompt = liftIO $ putStr "Command: " >> hFlush stdout
readdecode = liftIO $ getLine >>= return . parseExploreStr

interpret ecmd = case ecmd of
    Left e  -> liftIO $ putStrLn $ show e
    Right c -> interpret' c

interpret' :: ExpCommand -> Game ()
interpret' (Fen s) = do
    ha <- gets hash
    hi <- gets hist
    let st = posToState (posFromFen s) ha hi
    put st
interpret' Init = do
    ha <- gets hash
    hi <- gets hist
    let s = posToState initPos ha hi
    put s
interpret' Moves = genMoves 0 False >>= out . show
interpret' QMoves = genTactMoves >>= out . show
interpret' (Down m) = doMove False m False >> return ()
interpret' Up = undoMove undefined
interpret' Eval = staticVal0 >>= \x -> liftIO $ putStrLn ("static score = " ++ show x)
interpret' QEval = liftIO $ putStrLn "not inplemented!"
interpret' Help = liftIO printHelp
interpret' Exit = liftIO $ exitWith ExitSuccess

printHelp = do
    putStrLn "f <fen> - new position from fen string"
    putStrLn "i       - new position from start position"
    putStrLn "m       - print all possible moves"
    putStrLn "q       - print all possible quiescent moves"
    putStrLn "d <mv>  - down one move (i.e. doMove)"
    putStrLn "u       - up (i.e. undoMove - last move)"
    putStrLn "e       - eval position and print the static score"
    putStrLn "v       - qsearch, then eval & print result"
    putStrLn "h       - this help text"
    putStrLn "x       - exit program"
