module Main (main) where

import Control.Monad
import Data.Ord
import Data.List
import qualified Data.ByteString.Char8 as B
-- import System.Environment (getArgs)
-- import System.Random
import qualified Data.Vector.Unboxed as V

import Struct.Status
import Eval.Eval
import Eval.OptParams

main = do
    putStrLn "Reading the vectors..."
    samples <- readSamples "theVectors.txt"
    putStrLn $ show (length samples) ++ " samples read"
    putStrLn "Optimizing..."
    let (def, dist0) = initPars
    best <- optimize (fitness samples) (inLimits parLims) def dist0
    putStrLn "======================"
    putStrLn "Optimum is:"
    forM_ (zip paramNames best) $ \(n, v) -> putStrLn $ n ++ " = " ++ show v

readSamples fn = fmap B.lines (B.readFile fn) >>= mapM (return . perLine)
    -- >>= return . filter (not . null . snd)	-- gives out of memory :-(

perLine :: B.ByteString -> (Vec, [Vec])
perLine lin = v0 `seq` vs `seq` (v0, vs)
    -- where (vl0, vls) = read $ map (toEnum . fromIntegral) $ B.unpack lin
    where (vl0, vls) = read $ B.unpack lin
          v0 = V.fromList vl0
          vs = map V.fromList vls

minimize = True

fitness :: [(Vec, [Vec])] -> Vec -> IO Double
fitness samples p = return $! if minimize then val else -val
    where preferSmall = V.sum $ V.map abs p	-- to prefer small parameter vectors
          val = preferSmall + sum (map (poserr p) samples)

poserr = poserr1

-- This one to minimize the distance between the parent position and the best child
poserr1 :: Vec -> (Vec, [Vec]) -> Double
poserr1 p (v0, vs)
    | null vs   = 0
    | otherwise = d * d
    where d  = (v0 <*> p) - l1 
          l1 = maximum (map (p <*>) vs)

-- This one to minimize/maximize the distance the best and the worst child
poserr2 :: Vec -> (Vec, [Vec]) -> Double
poserr2 p (_, vs)
    | null vs   = 0
    | otherwise = d * d / pp
    where d  = l2 - l1
          l1 = minimum prs
          l2 = maximum prs
          prs = map (p <*>) vs
          pp = p <*> p

-- This one to minimize the distance between parent and best child and maximize
-- the distance between best and worst child
-- So the sum of first and inverse of second
poserr3 v vs = poserr1 v vs + 1 / (poserr2 v vs + 1)

u <*> v = V.sum $ V.zipWith (*) u v

initPars = (def, (parDim, (def, vars0)))
    where ies = initEvalState False []
          def = esDParams ies
          vars0 = map f parLims
          f (mi, ma) = max (abs mi) (abs ma)
