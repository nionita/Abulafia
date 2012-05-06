module Eval.FileParams (
    learnConfigFilePrefix,
    makeEvalState,
    fileToState
  ) where

-- import Control.Monad
import Data.Char (isSpace)
import Data.List (isPrefixOf, sortBy)
import Data.Ord (comparing)
import System.Directory
-- import System.Environment (getArgs)
-- import System.IO

import Struct.Status(EvalState)
import Config.ConfigClass
import Eval.Eval (initEvalState)

learnConfigFilePrefix :: String
learnConfigFilePrefix = "evalParamsL"

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState :: Config c => c -> String -> Maybe FilePath -> IO (FilePath, EvalState)
makeEvalState cfg pver argfile =
    case argfile of
        Just afn -> do	-- config file as argument: it must be an evolution play
            fex <- doesFileExist afn
            if fex then filState afn afn else defState
        Nothing  -> case evalConfigFile cfg pver of	-- from general config
                           Nothing -> defState
                           Just fn -> do
                               fex <- doesFileExist fn
                               if fex then filState fn "" else defState
    where defState = return ("", initEvalState [])

filState :: FilePath -> String -> IO (String, EvalState)
filState fn ident = do
    est <- fileToState fn
    return (ident, est)

fileToState :: FilePath -> IO EvalState
fileToState fn = fileToParams `fmap` readFile fn >>= return . initEvalState

evalConfigFile :: Config c => c -> String -> Maybe String
evalConfigFile cfg pver = getSParam cfg "evalParamsFile"
                       >>= \fn -> Just $ fn ++ "-" ++ pver ++ ".txt"

getLastEvalConfigFile :: IO (Maybe String)
getLastEvalConfigFile = do
    allFiles <- getCurrentDirectory >>= getDirectoryContents
    let allCFiles = filter (learnConfigFilePrefix `isPrefixOf`) allFiles
    if null allCFiles
       then return Nothing
       else do
            tstamps <- mapM getModificationTime allCFiles
            return $ Just $ head $ reverse $ map snd $ sortBy (comparing fst) $ zip tstamps allCFiles

fileToParams :: String -> [(String, Double)]
fileToParams = map readParam . nocomments . lines
    where nocomments = filter (not . iscomment)
          iscomment [] = True
          iscomment ('-':'-':_) = True
          iscomment (c:cs) | isSpace c = iscomment cs
          iscomment _ = False

readParam :: String -> (String, Double)
readParam s = let (ns, vs) = span (/= '=') s in (strip ns, cleanread vs)
    where strip = filter (not . isSpace)
          cleanread = read . tail . strip
