module Eval.OptParams (
        optimize,
        Vec
    ) where

import Control.Monad
import Data.Ord
import Data.List
import qualified Data.ByteString.Char8 as B
import System.Random
import qualified Data.Vector.Unboxed as V

type Vec = V.Vector Double	-- Vector in compact form
type LVec = [Double]		-- Vector in list form
type Distrib = (Int, (LVec, LVec))	-- distribution

maxLoops = 200	-- number of loops
popCount = 50	-- population count
eliteFrc = 10	-- percent of the population which is elite
distStep = 0.3	-- distribution changing step

optimize :: (Vec -> IO Double)	-- function to optimize
         -> (LVec -> LVec)		-- parameter restricting function
         -> LVec			-- best so far
         -> Distrib 		-- the current distribution for solution candidates
         -> IO LVec		-- the solution (best candidate found)
optimize vf rf ipar = optim vf rf maxLoops ipar Nothing

-- This is the optimizing procedure
optim :: (Vec -> IO Double)	-- function to optimize
      -> (LVec -> LVec)		-- parameter restricting function
      -> Int			-- number of loops to go
      -> LVec			-- best so far
      -> Maybe Double		-- optimum so far
      -> Distrib 		-- the current distribution for solution candidates
      -> IO LVec		-- the solution (best candidate found)
optim _  _  0     best _     _    = return best
optim vf rf loops best bestf dist = do
    putStrLn $ "Generate candidates with dist " ++ show dist
    vs <- genCandidates rf dist popCount
    sc <- mapM vf vs
    let ss = take eco $ sortBy (comparing snd) $ zip vs sc
        ds = newDist dist $ map fst ss
        (rbs, rbf) = head ss
        (bf, bs) = case bestf of
             Nothing -> (V.toList rbs, Just rbf)
             Just s  -> if rbf < s then (V.toList rbs, Just rbf) else (best, bestf)
        ml1 = loops - 1
    putStrLn "----------------------"
    putStrLn $ "*** Current best so far: " ++ show bf
    putStrLn $ "Step ended, min = " ++ show bs ++ ", steps to go: " ++ show ml1
    putStrLn "======================"
    optim vf rf ml1 bf bs ds

genCandidates :: (LVec -> LVec) -> Distrib -> Int -> IO [Vec]
genCandidates rf dist n = forM [1..n] $ \_ -> genOneCand rf dist

genOneCand rf (_, (means, vars))
    = fmap (V.fromList . rf) $ mapM (uncurry fitNormal) $ zip means $ map sqrt vars

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

eco  = popCount * eliteFrc `div` 100
fln  = fromIntegral eco
fln1 = fln - 1

-- Calculate the new distribution based on the elite samples
newDist :: Distrib -> [Vec] -> Distrib
newDist (ddim, (omeans, ovars)) vs = (ddim, (nmeans, nvars))
    where means = V.map ( / fln) $ foldr (V.zipWith (+)) zero vs
          vars  = V.map ( / fln1) $ foldr (V.zipWith (+) . V.zipWith f means) zero vs
          f x y = let d = x - y in d * d
          nmeans = moveTo omeans $ V.toList means
          nvars  = moveTo ovars  $ V.toList vars
          zero = V.fromList $ replicate ddim 0

moveTo old new = zipWith (+) (map (* ost) old) (map (* distStep) new)
    where ost = 1 - distStep
