module Main (main) where

import Control.Monad
import Data.Bits
import System.IO
import System.Random
import Criterion.Main

import Struct.Struct
import Struct.Status
import Hash.TransTab
import Config.ConfigClass
import Config.Config

-- main = mainQC
-- main = mainBench
main = mainGen

mainQC = checkProp

mainGen = do
    putStrLn $ "*** Generation 15"
    progMain 100000 15
    putStrLn $ "*** Generation 17"
    progMain 100000 17

mainBench = defaultMain [
           bench "sto ret" $ progMain 100000 0
       ]

progMain k g = do
    ha  <- newCache defaultConfig
    let zkeys = take k $ makeZKeys rws
        rws  = randoms stdgen
        stdgen = mkStdGen $ fromIntegral g
    sto ha g zkeys
    ret ha zkeys

makeZKeys (a1:a2:as) = makezkey a1 a2 : makeZKeys as

makezkey :: Int -> Int -> ZKey
makezkey w1 w2 = fromIntegral w1 `shiftL` 32 .|. fromIntegral w2

sto ha g zkeys = do
    mapM_ wrc zkeys
    where wrc z = writeCache ha z g (deep z) (typ z) (score z) (best z) (node z)

ret ha zkeys = do
    nf <- liftM sum $ mapM rec zkeys
    putStrLn $ "Not found: " ++ show nf
    where rec z = do
              mr <- readCache ha z
              case mr of
                  Nothing -> return 1	-- putStrLn $ " = " ++ show z ++ ": not found"
                  Just (de, ty, sc, mv, no) -> do
                      let de' = deep z
                          ty' = typ z
                          sc' = score z
                          mv' = best z
                          no' = node z
                      when (de /= de') $ differ z "Depth" de de'
                      when (ty /= ty') $ differ z "Type " ty ty'
                      when (sc /= sc') $ differ z "Score" sc sc'
                      when (mv /= mv') $ differ z "Move " mv mv'
                      when (no /= no') $ differ z "Nodes" no no'
                      return 0

score z = max (-20000) $ min 20000 $ fromIntegral $ z .&. 0xFFFF
typ   z = fromIntegral $ z .&. 0x3
deep  z = fromIntegral $ z .&. 0x1F
best  z = Move $ fromIntegral $ z .&. 0x7FFFF
node  z = fromIntegral $ z .&. 0x7FFFFFFF

differ z tx a b = putStrLn $ " # " ++ show z ++ " " ++ tx ++ " differ: " ++ show a
                      ++ " should be " ++ show b
