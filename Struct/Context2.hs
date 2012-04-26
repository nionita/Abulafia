module Struct.Context2 where

-- This is the context for running 2 engines of the same software
-- but maybe with diffenrent parameters in one process with no UCI
-- communication inbetween, which is intended for self pplay for
-- parameter tuning

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.State.Strict
import Control.Monad.Reader
import System.Time

import Struct.Struct
import Struct.Status
import Config.ConfigClass
import Search.SearchMonad hiding (lift)

data InfoToGui = Info {
                    infoDepth :: Int,
                    -- infoSelDepth :: Int,
                    infoTime :: Int,
                    infoNodes :: Int,
                    infoPv :: [Move],
                    infoScore :: Int
                }
                | InfoB {
                    infoPv :: [Move],
                    infoScore :: Int
                }
                | InfoD { infoDepth :: Int }
                | InfoCM {
                    infoMove :: Move,
                    infoCurMove :: Int
                }
                | InfoS { infoString :: String }

-- Game related statistics for one engine
data GameStats = GameStats {
         gsUsedTime  :: Int,	-- used time in ms
         gsRemTime   :: Int,	-- remaining time in ms
         gsMoveTime  :: Int,	-- time in ms per move
         gsUsedNodes :: Int,	-- used kilo nodes
         gsRemNodes  :: Int,	-- remaining kilo nodes
         gsMoveNodes :: Int	-- kilo nodes per move
     }

-- This is the context in which the other components run
-- it has a fix part, established at the start of the programm,
-- and a variable part (type Changing) which is kept in an MVar
data Context = Ctx {
        szero  :: Int,			-- second zero (programm start)
        logger :: Maybe (Chan String),  -- the logger channel (if any)
        writer :: Chan String,          -- the writer channel
        loglev :: Int,                  -- loglevel, 0 - no log
        change :: MVar Changing         -- the changing context
    }

-- This is the variable context part (global mutable context)
data Changing = Chg {
        config     :: GConfig,          -- the configuration component
        compThread :: Maybe ThreadId,	-- the search thread id
        crtPos     :: MyPos,		-- current position
        crtId      :: String,		-- id of current engine
        crtStatus  :: MyState,          -- state of the current engine (active)
        crtStats   :: GameStats,	-- game statistics for current engine
        nxtId      :: String,		-- id of other engine
        nxtStatus  :: MyState,          -- state of the other engine (passive)
        nxtStats   :: GameStats,	-- game statistics for other engine
        srchStrtMs :: Int               -- search start time (milliseconds)
    }

type CtxIO = ReaderT Context IO

-- Result of a search
type IterResult = ([Move], Int, [Move], MyState)

-- Current time in ms since programm start
currMilli :: CtxIO Int
currMilli = do
    TOD s ps <- liftIO getClockTime
    s0 <- gets szero
    return $ fromIntegral $ (s-s0)*1000 + ps `div` 1000000000

readChanging :: CtxIO Changing
readChanging = do
    ctx <- ask
    liftIO $ readMVar $ change ctx

modifyChanging :: (Changing -> Changing) -> CtxIO ()
modifyChanging f = do
    ctx <- ask
    liftIO $ modifyMVar_ (change ctx) (return . f)

getCfg :: CtxIO GConfig
getCfg = do
    chg <- readChanging
    return $ config chg

getIParamDef :: String -> Int -> CtxIO Int
getIParamDef pn d = do
    GConfig cfg <- getCfg
    return $ getIParamDefault cfg pn d

ctxLog :: String -> String -> CtxIO ()
ctxLog prf mes = do
    ctx <- ask
    liftIO $ logging (logger ctx) prf mes

logging mlchan prf mes =
    case mlchan of
        Just lchan -> do
            -- TOD s ps   <- liftIO getClockTime
            -- let cms = fromIntegral $ s*1000 + ps `div` 1000000000
            cms <- currMilli
            writeChan lchan $ show cms ++ " [" ++ prf ++ "]: " ++ mes
        Nothing    -> return ()

currentSecs = do
    TOD s _ <- getClockTime
    return s