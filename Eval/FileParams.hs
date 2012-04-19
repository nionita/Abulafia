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
-- When learning, take the latest eval file of the form "evalParamsL*.txt"
makeEvalState :: Config c => Bool -> c -> String -> Maybe FilePath -> IO (FilePath, EvalState)
makeEvalState learn cfg pver argfile =
    case argfile of
        Just afn -> do	-- config file as argument: it must be an evolution play
            fex <- doesFileExist afn
            if fex then filState afn afn learn else defState
        Nothing  -> if learn	-- config from an earlier learned param file
                    then do
                         mecfn <- getLastEvalConfigFile
                         case mecfn of
                           Just ecfn -> filState ecfn "" learn
                           Nothing   -> defState
                    else case evalConfigFile cfg pver of	-- from general config
                           Nothing -> defState
                           Just fn -> do
                               fex <- doesFileExist fn
                               if fex then filState fn "" learn else defState
    where defState = return ("", initEvalState learn [])

filState :: FilePath -> String -> Bool -> IO (String, EvalState)
filState fn ident learn = do
    est <- fileToState learn fn
    return (ident, est)

fileToState :: Bool -> FilePath -> IO EvalState
fileToState learn fn = fileToParams `fmap` readFile fn >>= return . initEvalState learn

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
