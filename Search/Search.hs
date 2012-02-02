module Search.Search where

import Search.TreeState

-- First: alpha/beta
data TreeVars = TreeVars {
         draft :: Int,		-- draft of the search, readonly
         stans :: Int		-- statistic: total node count
     }

data LevelVars = LevelVars {
         level, depth :: Int,	-- level of root is 0; level + depth = draft
         curmv        :: Move,	-- current move searched at this level
         alpha, beta  :: Int,	-- current alpha & beta
         moves, path  :: [Move]	-- remaining moves and best path
     }

data ResVars = ResVars {
         rscor :: Int,		-- the score of the last node return
         rpath :: [Move],	-- path of the last node return
     }

dummyRes = ResVars { rscor = 0, rpath = [] }	-- used when we don't need a result

-- Actually this is search and qsearch in one
search :: Monad m => Int -> Int -> Int -> m (Int, [Move])
search alpha beta draft = do
    es <- genEdges	-- generate the root moves
    let tv = TreeVars  { draft = draft, stans = 1 }
        lv = LevelVars { level = 0, depth = draft, alpha = alpha, beta = beta, moves = es }
    traverse tv lv ResVars firstMove nextMove getRes

-- Called in preparing the first move of the current level
-- (i.e. the first node of the next level, aka one level deeper)
firstMove _ = do
    lv <- getLevelState
    case undefined of
    _ | depth lv == 0   -> qSearch	-- the frontier is reached
      | null (moves lv) -> finalNode	-- we have no valid moves
      | otherwise       -> nextLevel >>= noRes	-- go deeper

-- Second and further moves at the same level
nextMove r = do
    lift undoMove	-- undo the previous move
    modifyTreeState $ \t -> t { stans = stans t + 1 }	-- one more node after undo move
    lv <- getLevelState
    if (rscor r >= beta lv)
       then betaCut r
       else do
          when (rscor r > alpha lv) $
              modifyLevelState $ \l -> l { alpha = rscor r, path = curmv l : rpath r }
          case moves lv of
              [] -> bestPath 
              _  -> nextLevel

-- When a beta cut occurs, we abort the current level
-- and return beta (negated) and the refutation path
betaCut r = do
  lv <- getLevelState
  return $ Left r { rscor = - beta lv, rpath = curmv lv : rpath r }

-- When returning from a deeper level, we need to setup the result
-- to contain the (negated) score and path of that level
bestPath = do
    lv <- getLevelState
    return $ ResVars { rscor = - alpha lv, rpath = path lv }

-- Make move to go to the next level
-- Here we always have a next move (moves lv /= [])
nextLevel = do
    lv <- getLevelState
    let (e : es) = moves lv
    putLevelState lv { curmv = e, moves = es }
    lift $ doMove e
    nes <- if depth lv > 1
              then lift genMoves	-- next level is at least depth 1
              else lift genTactMoves	-- next level is depth <= 0
    let lv' = LevelVars { level = level lv + 1, depth = depth lv - 1, curmv = e,	-- e!!!
                          alpha = - beta lv, beta = - alpha lv, moves = nes, path = [] }
    return $ Right lv'

-- As long as the 2 functions (firstMove and nextMove) need to return
-- different types, we need this helper
noRes x = case x of
              Left x'  -> Left x'
              Right x' -> Right (x', undefined)

finalNode = do
    v <- lift static	-- the static evaluation
    return $ Left ResVars { rscor = v, rpath = [] }
