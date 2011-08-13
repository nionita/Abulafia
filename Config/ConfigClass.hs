{-# LANGUAGE ExistentialQuantification #-}
module Config.ConfigClass where
import Data.Maybe
import System.IO

type ParamName = String

type FileName = String
class Config c where
    fromFile :: FileName -> IO c
    setIParam :: c -> ParamName -> Int -> c
    getIParam :: c -> ParamName -> Maybe Int
    setSParam :: c -> ParamName -> String -> c
    getSParam :: c -> ParamName -> Maybe String
    getIParamDefault :: c -> ParamName -> Int -> Int
    getIParamDefault c pn d = fromMaybe d $ getIParam c pn
    getSParamDefault :: c -> ParamName -> String -> String
    getSParamDefault c pn d = fromMaybe d $ getSParam c pn

-- A generic config to store in our context
data GConfig = forall c. Config c => GConfig c
