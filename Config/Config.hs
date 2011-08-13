module Config.Config where
import qualified Data.Map as M
import Config.ConfigClass

data MConfig = MConfig {
                  intParams :: M.Map String Int,
                  strParams :: M.Map String String
              }

defIParams :: M.Map String Int
defIParams = M.fromList [
    ("logLevel", 0),            -- log level (what else?)
    -- ("ttSize", 10 * 1000 * 1000),    -- no of entries in the transposition table
    ("ttSize", 2 ^ 20),    -- no of entries in the transposition table
    ("ttWindow", 0),	-- protected window in the transposition table
    -- ("hashSize", 1),    -- entries in the hash
    ("firstDepth", 1),           -- first search depth in iterative deepening
    ("maxDepth", 20),            -- a search safe limit :-)
    ("graceMs", 200)             -- min search time in ms
    ]

defSParams :: M.Map String String
defSParams = M.fromList [
        ("evalParamsFile", "evalParams")	-- prefix of the eval parameters file name
    ]
 
defaultConfig = MConfig defIParams defSParams

instance Config MConfig where
    fromFile  = readFromFile 
    setIParam = setIntParam
    getIParam = getIntParam
    setSParam = setStrParam
    getSParam = getStrParam

readFromFile f = return defaultConfig   -- temporary

setIntParam (MConfig ip sp) pname pval = MConfig ip1 sp
    where ip1 = M.insert pname pval ip

getIntParam (MConfig ip _) pname = M.lookup pname ip

setStrParam (MConfig ip sp) pname pval = MConfig ip sp1
    where sp1 = M.insert pname pval sp

getStrParam (MConfig _ sp) pname = M.lookup pname sp
