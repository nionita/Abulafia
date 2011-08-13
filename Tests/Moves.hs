{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Monad (when, liftM)
import System.Time
import System.Exit
import System.Environment (getArgs)
import Data.Bits

import Struct.Struct
import Moves.Moves
import Moves.Board
import Moves.SEE
import Moves.Base

main = fmap head getArgs >>= readFile >>= return . lines >>= mapM_ perPos
    
perPos :: String -> IO ()
perPos fen = do
    putStrLn $ "Fen: " ++ fen
    let pos = updatePos True $ posFromFen fen
        -- movs = map (uncurry moveFromTo) $ genMoveCaptSEE pos (moving pos)
        (gwmovs, bwmovs) = genMoveCaptWL pos White
        (gbmovs, bbmovs) = genMoveCaptWL pos Black
    putStrLn $ showMyPos pos
    putStrLn "Good moves for white:"
    putStrLn $ show $ map (uncurry moveFromTo) gwmovs
    putStrLn "Bad  moves for white:"
    putStrLn $ show $ map (uncurry moveFromTo) bwmovs
    putStrLn "Good moves for black:"
    putStrLn $ show $ map (uncurry moveFromTo) gbmovs
    putStrLn "Bad  moves for black:"
    putStrLn $ show $ map (uncurry moveFromTo) bbmovs
    putStrLn $ take 40 $ repeat '='
