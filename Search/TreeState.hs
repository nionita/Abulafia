{-# LANGUAGE NoMonomorphismRestriction #-}
module Search.TreeState
  -- (
  -- DynTree(..),
  -- LevelInitFunc, NodeInitFunc,
  -- traverse
  -- )
  where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Class (lift)

-- State while traversing a tree:
-- - tree  state - initialized before tree traversal, visible in the whole tree
-- - level state - initialized when enter a level (root distance) first time, visible only at that level
-- (only nodes descending from the same parent are considered to be at the same level)
-- The initial tree state is give to the traverse function as a parameter.
-- For the level state initialisation a user function must be supplied

-- Generic tree traversal implementation based on the state monad
-- This is our traversal state
data Adm t l r = Adm {
         trees :: t,	-- tree state
         lstck :: [l],	-- level stack
         lastr :: r	-- last result from an init function
     } deriving Show

-- We could use the CC implementation of the state monad to go faster
type TreeState t l r m = StateT (Adm t l r) m

getTreeState  = gets trees
getLevelState = gets lstck >>= return . head
putTreeState  t = modify $ \s -> s { trees = t }
putLevelState l = modify $ \s -> s { lstck = l : tail (lstck s) }

modifyTreeState  f = modify $ \s -> s { trees = f $ trees s }
modifyLevelState f = modify $ \s -> let (l : ls) = lstck s in s { lstck = f l : ls }

-- These functions are called in preparation of the next level or the next node
-- Important remarks for semantic:
-- 1. the function for initialising a new level (and the first node in it) has access
--    (and can modify) the parent level substatus from the point of view of the new
--    level and node
-- 2. the function for initialising a new (sibling) node has access (and can modify) the
--    current level substatus from the point of view of the new node; however, the
--    modification of the level status in the last node will have
--    effect only during the action itself, because afterwards the level substatus will be
--    lost (by changing back to the lower towards root)
-- 3. for level 0 there will be no next sibling initialisation call, as root has no sibling
type LevelInitFunc t l r m = r -> TreeState t l r m (Either r (l, r))
type NodeInitFunc  t l r m = r -> TreeState t l r m (Either r r)

-- This action goes up to the next level, if any
up :: Monad m => LevelInitFunc t l r m -> NodeInitFunc t l r m -> TreeState t l r m ()
up fl fn = do
  mln <- gets lastr >>= fl	-- new level?
  case mln of
    Left r       -> modify (\s -> s { lastr = r })                      >> next fl fn
    Right (l, r) -> modify (\s -> s { lstck = l : lstck s, lastr = r }) >> up   fl fn

-- This action goes right to the next sibling (if not on level 0)
next :: Monad m => LevelInitFunc t l n m -> NodeInitFunc t l n m -> TreeState t l n m ()
next fl fn = do
  isl0 <- isLevel0
  if isl0
     then return ()		-- the tree traversal is ended
     else do
       mn <- gets lastr >>= fn	-- next node (sibling)?
       case mn of
         Left  r -> modify (\s -> s { lastr = r }) >> down fl fn
         Right r -> modify (\s -> s { lastr = r }) >> up   fl fn

-- This action goes down one level toward the root
-- It will never be called in level 0 (see function next)
down :: Monad m => LevelInitFunc t l r m -> NodeInitFunc t l r m -> TreeState t l r m ()
down fl fn = modify (\s -> s { lstck = tail $ lstck s }) >> next fl fn

isLevel0 :: Monad m => TreeState t l r m Bool
isLevel0 = gets lstck >>= return . null

-- A tree traversal takes place in a user monad and is depth first
-- When entering a tree traversal, we need these parameters:
-- + the initial tree substate
-- + the initial level substate for level 0 (i.e. root)
-- + the initial "result" for the first initialization function
-- + a monadic action to calculate the level substate from the tree and level substates
-- + a monadic action to be called when the next node in an existing level is created
-- + a monadic action to extract information from the last tree and level substates encountered
--   in the traversal
traverse :: Monad m => t -> l -> r	-- initial substates (for tree and level 0) and result
         -> LevelInitFunc t l r m	-- level initialisation action
         -> NodeInitFunc  t l r m	-- node initialisation action
         -> (t -> l -> r -> m a)	-- action to extract the results
         -> m a
traverse t l r fl fn fr = do
  -- execute the traversal in the state monad from the initial state
  adm <- execStateT (up fl fn) $ Adm { trees = t, lastr = r, lstck = [l] }
  -- extract the results
  fr (trees adm) (head $ lstck adm) (lastr adm)

-- Example and test:
-- This should (create and) traverse a tree with m levels (from 0 to m-1), with every node in levels 0
-- to m-2 having n child nodes each (level m-1 contains only leaves), while counting the total number of nodes
-- (which should result in 1 + n + n^2 + ... + n^(m - 1) = (n^m - 1) / (n - 1)).
-- It can run in any monad, for example in IO:
-- treecount 3 2 >>= print	-- should print 7
-- Substates:
-- tree:  number of nodes counted so far
-- level: level number (0 for root), siblings left for this level
-- node:  nothing
-- treecount :: Monad m => Int -> Int -> m Int
treecount m n = traverse 1 (0, 0) () (treelevel m n) (treenode m n) (\t _ _ -> return t)

-- Initialize new level
-- treelevel :: Monad m => Int -> Int -> TreeState Int (Int, Int) () m (Maybe ((Int, Int), ()))
treelevel m n _ = do
  (l, _) <- getLevelState	-- current level
  -- lift $ print $ "treelevel " ++ show l
  get >>= lift . print
  if l >= m - 1
     then return (Left ())	-- current level is maximum, no further level
     else do
       -- lift $ print $ "Level " ++ show (l+1) ++ " node 1"
       modifyTreeState $ \t -> t + 1	-- one more node created (the first on the new level)
       return $ Right ((l+1, n-1), ())	-- next level, n-1 more children at that level

-- Initialize new (sibling) node
-- treenode :: Monad m => Int -> Int -> TreeState Int (Int, Int) () m (Maybe ())
treenode _ n _ = do
  (l, r) <- getLevelState	-- remaining nodes in current level
  -- lift $ print $ "treenode " ++ show l ++ " / " ++ show r
  if r <= 0
     then return (Left ())	-- no further node here
     else do
       -- lift $ print $ "Level " ++ show l ++ " node " ++ show (n - r + 1)
       modifyTreeState $ \t -> t + 1		-- one more node here
       putLevelState (l, r-1)	-- level nodes go down
       return $ Right ()
