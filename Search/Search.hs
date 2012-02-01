module Search where

import Search.TreeState

-- First alpha/beta:
data TreeVars = TreeVars {
         draft :: Int,		-- draft of the search, readonly
         lscor :: Int,		-- the score of the last node return
         lpath :: [Move],	-- path of the last node return
         stans :: Int		-- statistic: total node count
     }

data LevelVars = LevelVars {
         level, depth :: Int,	-- level of root is 0; level + depth = draft
         alpha, beta  :: Int,	-- current alpha & beta
         moves, path  :: [Move]	-- remaining moves and best path
     }

data NodeVars = NodeVars

search :: Monad m => Int -> Int -> Int -> m (Int, [Move])
search alpha beta draft = do
    es <- genEdges	-- generate the root moves
    let tv = TreeVars  { draft = draft, lscor = 0, lpath = [], stans = 1 }
        lv = LevelVars { level = 0, depth = draft, alpha = alpha, beta = beta, moves = es }
    traverse tv lv NodeVars firstMove nextMove getRes

firstMove = do
    lv <- getLevelState
    case undefined of
    _ | depth lv == 0   -> qSearch
      | null (moves lv) -> finalNode
      | otherwise       -> nextLevel

-- Make move to go to the next level
-- Here we always have a next move (moves lv /= [])
nextLevel = do
    lv <- getLevelState
    let (e : es) = moves lv
        lv' = LevelVars { level = level lv + 1, depth = depth lv - 1,
                          alpha = - beta lv, beta = - alpha lv, moves = es }
    lift $ doMove e
    return $ Just (lv', NodeVars)

-- Second and further moves at the same level
nextMove = do
    lift undoMove	-- undo the previous move
    tv <- getTreeState
    lv <- getLevelState
    if (lscor tv >= beta lv)
       then betaCut
       else do
          when (lscor tv > alpha lv) $ do
              modifyLevelState $ \l -> l { alpha = lscor tv }
          if null (moves lv)
             then do
                 modifyTreeState $ \t -> t { lscor = alpha lv }
                 return Nothing
             else nextLevel
