module Eval.Gradient (
    nextGradStep, minStep, (<+>), (<*>)
) where

import Data.Array.Unboxed

-- Calculate the next gradient step size based on the angle between the last
-- two error vectors: if the angle is small, the step will increase, because
-- it looks we go in the right direction, otherwise the step will be lowered
-- The function gives also back the mean value of the error vectors
-- and the angle of them
nextGradStep :: Double -> [Double] -> [Double] -> (Double, Double, Double)
nextGradStep ostep vec1 vec2
    | amt1 < eps || amt2 < eps = (ostep, meane, 0)
    | rcos >= rcosAccel        = (nstepa, meane, angle)
    | rcos >= rcosEqual        = (ostep, meane, angle)
    | rcos >= rcosHalf         = (nstepd, meane, angle)
    | otherwise                = (minStep, meane, angle)
    where num  = vec1 <*> vec2
          den1 = vec1 <*> vec1
          den2 = vec2 <*> vec2
          amt1 = sqrt den1
          amt2 = sqrt den2
          meane = (amt1 + amt1) / 2
          rcos = num / amt1 / amt2
          angle = acos $ between (-1) 1 rcos
          -- icos = round $ 2.25 * rcos
          -- fact = (factors!icos) `between` minStep maxStep
          -- nstep = ostep * fact
          -- nstepa = min maxStep $ ostep * 1.5
          nstepa = min maxStep $ ostep + minStep
          nstepd = max minStep $ ostep / 2

between mi ma x = max mi $ min ma x

rcosAccel = 0.99
rcosEqual = 0.95
rcosHalf  = 0.85

maxStep = 1.0e-5
minStep = 1.0e-6
eps = 1.0e-10

factors :: UArray Int Double
factors = array (-2, 2) [ (-2, 0.7), (-1, 0.8), (0, 0.9), (1, 1), (2, 1.2)]


(<+>) :: Num a => [a] -> [a] -> [a]
(<+>) = zipWith (+)

(<*>) :: Num a => [a] -> [a] -> a
(<*>) a b = sum $ zipWith (*) a b
{-# SPECIALIZE (<*>) :: [Int] -> [Int] -> Int #-}
