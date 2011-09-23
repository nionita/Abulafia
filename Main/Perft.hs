{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Monad (when)
import System.Time
import System.Exit
import System.Environment (getArgs)
import Data.Bits

import Struct.Struct
import Moves.Moves
import Moves.Board

main = do
    args <- getArgs
    let p = updatePos initPos
        n = case args of
                (n1:_) -> read n1
                _      -> 3
        c = White
        r = perft p c n
    putStrLn "Initialize..."
    t0 <- getClockTime
    when (whAttacs p `seq` movesInit == 0) $ do
        putStrLn "Initialization fails!"
        exitWith $ ExitFailure 1
    t1 <- getClockTime
    putStrLn $ "Initialization took " ++ showTimeDiff t1 t0
    putStrLn $ "Compute perft level " ++ show n
    t2 <- getClockTime
    when (r == 0) $ return ()	-- to evaluate r
    t3 <- getClockTime
    putStrLn $ "Result is: " ++ show r
    putStrLn $ "Computation took " ++ showTimeDiff t3 t2

-- perft, perft' :: MyPos -> Color -> Int -> Int
perft pos !col !depth = perft' pos col depth
perft'  _   _   0     = 1
perft' !pos col depth = sum $ map f mvs
    where f m = perft (doFromToMove m pos) (other col) (depth - 1)
          l1 = map (genmvT pos) $ genMoveTransf pos col
          l2 = map (genmv True pos) $ genMoveCapt pos col ++ genMoveNCapt pos col
                              ++ genMovePCapt pos col ++ genMovePNCapt pos col
          l3 = map (genmv True pos) $ genMoveFCheck pos col
          mvs = if isCheck pos col then l3 else l1 ++ l2

-- perfti _   _   0     = 1
-- perfti pos col depth = sum $ map f $ genMoveCapt pos col ++ genMoveNCapt pos col
--     where f (s, d) = perfti (doFromToMove s d pos) (other col) (depth - 1)

-- {-# INLINE inCheck #-}
-- inCheck :: MyPos -> Color -> Bool
-- inCheck p c = pieces .&. kings p .&. attacs /= 0
--     where (pieces, attacs) = if c == White then (white p, blAttacs p) else (black p, whAttacs p)

showTimeDiff (TOD s1 p1) (TOD s2 p2) = show (fst td) ++ "." ++ show (snd td) ++ " seconds"
    where tds = s1 - s2
          tdp = p1 - p2
          td = if tdp < 0 then (tds - 1, 1000000000000 + tdp) else (tds, tdp)
