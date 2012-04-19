module Main where
import Uci.UciMain
-- import Uci.UCI
import Config.ConfigClass
import Config.Config

main :: IO ()
main = uciMain (GConfig defaultConfig)
