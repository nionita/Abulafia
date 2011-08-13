module Main (main) where

import Control.Monad
import Control.Monad.State (evalState)
import qualified Data.ByteString.Char8 as B
-- import System.Time
import System.Environment (getArgs)
import System.IO
-- import Data.Bits

import Struct.Struct
import Struct.Status
import Moves.Moves
import Moves.Board
import Moves.Base
import Moves.History
import Eval.Eval
import Hash.SimpleCache
import Search.Albeta (Comm(..))
import Search.SearchMonad
import Config.ConfigClass
import Config.Config

-- Here we work in the old plain IO monad:
instance CtxMon IO where
    tellCtx = tellInIO

type Feats = [Int]

data Once = Once Cache History EvalState

main = do
    ha  <- newCache defaultConfig
    hi  <- newHist
    let evs = initEvalState False []
    fn <- getArgs >>= return . head
    ih <- openFile fn ReadMode
    oh <- openFile "theVectors.txt" WriteMode
    goReadWrite ih oh (perPos (Once ha hi evs))
    -- B.readFile >>= return . B.lines
    --    >>= mapM (perPos (Once ha hi evs)) >>= mapM_ (B.hPut ioh)
    -- writeFile "theVectors.txt" (unlines lins)
    hClose oh

goReadWrite :: Handle -> Handle -> (B.ByteString -> IO B.ByteString) -> IO ()
goReadWrite ih oh f = do
    eof <- hIsEOF ih
    when (not eof) $ do
        lin <- B.hGetLine ih >>= f >>= B.hPut oh
        goReadWrite ih oh f

perPos :: Once -> B.ByteString -> IO B.ByteString
perPos (Once ha hi evs) fen' = do
    let fen = B.unpack fen'
        pos = updatePos True $ posFromFen fen
        inist = posToState pos ha hi evs
    putStrLn $ "Fen: " ++ fen
    -- Here: we should check if it's a final status or so...
    (r, finst) <- runSearch (oneLevel pos) inist
    return $ B.pack $ show r ++ "\n"

oneLevel pos = do
    s <- get
    t <- getPos
    let c = moving pos
        (_, feats0) = evalState (posEval pos c) (evalst s)
    mvs <- genMoves 0 0 False >>= return . filter (quiet t)	-- only quiescent moves
    ff  <- forM mvs $ \m -> do
        r <- doMove False m False
        -- Here: we should check if it's a final status or so...
        t <- getPos
        let feats = staticFeats t
        undoMove m
        return feats
    return (feats0, ff)

quiet :: MyPos -> Move -> Bool
quiet p m = not (moveIsEnPas m || moveIsTransf m) && tabla p (toSquare m) == Empty

tellInIO :: Comm Move Int -> IO ()
tellInIO (LogMes s) = putStrLn $ "Info: " ++ s
tellInIO _ = return ()
