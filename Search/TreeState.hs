{-# LANGUAGE NoMonomorphismRestriction #-}
module TreeState
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
     }

-- We could use the CC implementation of the state monad to go faster
type TreeState t l r m = StateT (Adm t l r) m

getTreeState  = gets trees
getLevelState = gets lstck >>= return . head
putTreeState  t = modify $ \s -> s { trees = t }
putLevelState l = modify $ \s -> s { lstck = l : lstck s }

modifyTreeState  f = modify $ \s -> s { trees = f $ trees s }
modifyLevelState f = modify $ \s -> let (l : ls) = lstck s in s { lstck = f l : ls }

-- These functions are called in preparation of the next level or the next node
-- Important remarks for semantic:
-- 1. the function for initialising a new level (and the first node in it) has access
--    (and can modify) the parent level substatus and parend node substatus from the point of
--    view of the new level and node
-- 2. the function for initialising a new (sibling) node has access (and can modify) the
--    current level substatus and the previous sibling node substatus from the point of
--    view of the new node; however, the modification of the previous node status will have
--    effect only during the action itself, because afterwards the node substatus will be
--    replaced by the new created node substatus (or by the parend node, if no new sibling
--    will be created)
type LevelInitFunc t l r m = r -> TreeState t l r m (Either r (l, r))
type NodeInitFunc  t l r m = r -> TreeState t l r m (Either r r)

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

-- This action goes up to the next level, if any
up :: Monad m => LevelInitFunc t l r m -> NodeInitFunc t l r m -> TreeState t l r m ()
up fl fn = do
  mln <- gets lastr >>= fl	-- init new level
  case mln of
    Left r       -> modify (\s -> s { lastr = r }) >> next fl fn	-- no new level (leaf) - next node
    Right (l, r) -> modify (\s -> s { lstck = l : lstck s, lastr = r }) >> up fl fn

-- This actions goes right to the next sibling
next :: Monad m => LevelInitFunc t l n m -> NodeInitFunc t l n m -> TreeState t l n m ()
next fl fn = do
  mn <- gets lastr >>= fn	-- next node (sibling)
  case mn of
    Left  r -> modify (\s -> s { lastr = r }) >> down fl fn
    Right r -> modify (\s -> s { lastr = r }) >> up fl fn

-- This action goes down one level toward the root
down :: Monad m => LevelInitFunc t l n m -> NodeInitFunc t l n m -> TreeState t l n m ()
down fl fn = do
  ls' <- gets lstck
  case ls' of
    []       -> return ()	-- the tree traversal is ended
    (_ : ls) -> modify (\s -> s { lstck = ls }) >> next fl fn

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
  lift $ print $ "treelevel"
  (l, _) <- getLevelState	-- current level
  if l <= m - 1
     then return $ Left ()	-- current level is maximum, no further level
     else do
       lift $ print $ "Level " ++ show (l+1) ++ " node 1"
       modifyTreeState $ \t -> t + 1	-- one more node created (the first on the new level)
       return $ Right ((l+1, n-1), ())	-- next level, n-1 more child at that level

-- Initialize new (sibling) node
-- treenode :: Monad m => Int -> Int -> TreeState Int (Int, Int) () m (Maybe ())
treenode _ n _ = do
  lift $ print $ "treenode"
  (l, r) <- getLevelState	-- remaining nodes in current level
  if r <= 0
     then return $ Left ()	-- no further node here
     else do
       lift $ print $ "Level " ++ show l ++ " node " ++ show (n - r + 1)
       modifyTreeState $ \t -> t + 1		-- one more node here
       putLevelState (l, r-1)	-- level nodes go down
       return $ Right ()
