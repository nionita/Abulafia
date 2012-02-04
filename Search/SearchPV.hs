module Search.SearchPV where

import Search.TreeState

-- First: alpha/beta
data TreeVars = TreeVars {
         draft :: Int,		-- draft of the search, readonly
         stans :: Int,		-- statistic: total node count
         staqs :: Int,		-- statistic: total nodes in qsearch
         maxnd :: Int		-- maximum negative depth (for selective depth)
     }

data LevelVars = LevelVars {
         level, depth :: Int,	-- level of root is 0; level + depth = draft
         curmv        :: Move,	-- current move searched at this level
         alpha, beta  :: Int,	-- current alpha & beta
         forpv, rese  :: Bool,	-- searching for PV? Research?
         moves, path  :: [Move]	-- remaining moves and best path
     }

lvdef = LevelVars { level = 0, depth = 0, curmv = __, alpha = 0, beta = 0,
                    forpv = True, rese = False, moves = [], path = [] }

data ResVars = ResVars {
         rscor :: Int,		-- the score of the last node return
         rpath :: [Move],	-- path of the last node return
     }

__ = undefined

-- Actually this is search and qsearch in one
search :: Monad m => Int -> Int -> Int -> m (Int, [Move])
search alpha beta draft = do
    es <- genEdges	-- generate the root moves
    let tv = TreeVars  { draft = draft, stans = 1, staqs = 0, maxnd = 0 }
        lv = lvdef { depth = draft, alpha = alpha, beta = beta, moves = es }
    traverse tv lv __ firstMove nextMove getRes

-- Called in preparing the first move of the current level
-- (i.e. the first node of the next level, aka one level deeper)
firstMove _ = do
    lv <- getLevelState
    if null (moves lv)
       then finalNode	-- we have no valid moves
       else nextLevel >>= noRes	-- go deeper

-- Second and further moves at the same level
nextMove r = do
    lift undoMove	-- undo the previous move
    lv <- getLevelState
    -- update node count after undo move
    if depth lv <= 0
       then modifyTreeState $ \t -> t { stans = stans t + 1, staqs = staqs t + 1 }	
       else modifyTreeState $ \t -> t { stans = stans t + 1 }
    if forpv lv && rscor r > alpha lv
       then do
           putLevelState lv { rese = True }
           nextLevel
       else do
       modifyLevelState \l -> l { moves = tail (moves l) }
       if rscor r >= beta lv
               then betaCut r
               else do
                   when (rscor r > alpha lv) $
                       modifyLevelState $ \l -> l { alpha = rscor r, forpv = False,
                                                    path = curmv l : rpath r }
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
    let e = head $ moves lv
    putLevelState lv { curmv = e }
    lift $ doMove e
    nes <- if depth lv > 1
              then lift genMoves	-- next level is at least depth >= 1
              else lift genTactMoves	-- next level is depth <= 0	-- start of qearch
    let beta' = if forpv lv || rese lv then beta lv else alpha lv + 1
        lv'   = lvdef { level = level lv + 1, depth = depth lv - 1,
                      alpha = - beta', beta = - alpha lv, moves = nes }
    modifyTreeState $ \t -> let maxd = depth lv - 1
                            in if maxd < maxnd t then t { maxnd = maxd } else t
    return $ Right lv'

-- As long as the 2 functions (firstMove and nextMove) need to return
-- different types, we need this helper
noRes x = case x of
              Left x'  -> Left x'
              Right x' -> Right (x', undefined)

finalNode = do
    v <- lift static	-- the static evaluation
    return $ Left ResVars { rscor = v, rpath = [] }
