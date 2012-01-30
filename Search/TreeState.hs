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
-- - node  state - initialized when enter a node first time, visible only at that node
-- The initialization order is: tree, level, node. For example, before the root node variables
-- are set up, the tree variables are initialized and then the level 0 variables
-- During the tree traversal there are always three sets of variables which we see (and can modify):
-- The tree, the current level and the current node variables

-- Generic tree traversal implementation based on the state monad
-- This is our traversal state
data Adm t l n = Adm {
         trees :: t,	-- tree state
         leves :: l,	-- current level state
         nodes :: n,	-- current node state
         lstck :: [l],	-- level stack
         nstck :: [n]	-- node stack
     }

-- We could use the CC implementation of the state monad to go faster
type TreeState t l n m = StateT (Adm t l n) m

{--
-- Class for tree traversal with operations resembling the state monad operations,
-- but specialized for the substates
class (Monad m, Monad ts t l n m) =>
  DynTree ts t l n m where
    getTreeState  :: ts t l n m t
    getLevelState :: ts t l n m l
    getNodeState  :: ts t l n m n
    putTreeState  :: t -> ts t l n m ()
    putLevelState :: l -> ts t l n m ()
    putNodeState  :: n -> ts t l n m ()

instance DynTree TreeState t l n m where
--}

getTreeState  = gets trees
getLevelState = gets leves
getNodeState  = gets nodes
putTreeState  t = modify $ \s -> s { trees = t }
putLevelState l = modify $ \s -> s { leves = l }
putNodeState  n = modify $ \s -> s { nodes = n }

-- modifyTreeState  f = getTreeState  >>= putTreeState  . f	-- modify $ \s -> s { trees = f t }
-- modifyLevelState f = getLevelState >>= putLevelState . f	-- modify $ \s -> s { leves = f l }
-- modifyNodeState  f = getNodeState  >>= putNodeState  . f	-- modify $ \s -> s { nodes = f n }
modifyTreeState  f = modify $ \s -> s { trees = f $ trees s }
modifyLevelState f = modify $ \s -> s { leves = f $ leves s }
modifyNodeState  f = modify $ \s -> s { nodes = f $ nodes s }

-- These functions prepare the next level or the next node
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
type LevelInitFunc t l n m = TreeState t l n m (Maybe (l, n))
type NodeInitFunc  t l n m = TreeState t l n m (Maybe n)

-- A tree traversal takes place in a user monad and is depth first
-- When entering a tree traversal, we need these parameters:
-- + the initial tree substate
-- + the initial level substate for level 0 (i.e. root)
-- + the initial node substate for root
-- + a monadic action to calculate the level substate from the tree, level and node substates
-- + a monadic action to calculate the node substate from the tree, level and node substates
-- + a monadic action to extract information from the last tree, level and node substates encountered
--   in the traversal
traverse :: Monad m => t -> l -> n	-- initial substates (for tree, level 0, root node)
         -> LevelInitFunc t l n m	-- level initialisation action
         -> NodeInitFunc  t l n m	-- node initialisation action
         -> (t -> l -> n -> m a)	-- action to extract the results
         -> m a
traverse t l n fl fn fr = do
  -- execute the traversal in the state monad from the initial state
  adm <- execStateT (up fl fn) $ Adm { trees = t, leves = l, nodes = n, lstck = [], nstck = [] }
  -- extract the results
  fr (trees adm) (leves adm) (nodes adm)

-- This action goes up to the next level, if any
up :: Monad m => LevelInitFunc t l n m -> NodeInitFunc t l n m -> TreeState t l n m ()
up fl fn = do
  mln <- fl	-- init new level
  case mln of
    Nothing     -> next fl fn	-- no new level (leaf node) - next node
    Just (l, n) -> do
      modify $ \s -> s { leves = l, nodes = n, lstck = leves s : lstck s, nstck = nodes s : nstck s }
      up fl fn

-- This actions goes right to the next sibling
next :: Monad m => LevelInitFunc t l n m -> NodeInitFunc t l n m -> TreeState t l n m ()
next fl fn = do
  mn <- fn	-- next node (sibling)
  case mn of
    Nothing -> down fl fn
    Just n  -> do
      modify $ \s -> s { nodes = n }
      up fl fn

-- This action goes down one level toward the root
down :: Monad m => LevelInitFunc t l n m -> NodeInitFunc t l n m -> TreeState t l n m ()
down fl fn = do
  ls' <- gets lstck
  case ls' of
    []        -> return ()	-- the tree traversal is ended
    (lh : ls) -> do
      modify $ \s -> let (nh : ns) = nstck s
                     in s { leves = lh, nodes = nh, lstck = ls, nstck = ns }
      next fl fn

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
treelevel m n = do
  (l, _) <- getLevelState	-- current level
  if l == m - 1
     then return Nothing		-- current level is maximum, no further level
     else do
       lift $ print $ "Level " ++ show (l+1) ++ " node 1"
       modifyTreeState $ \t -> t + 1	-- one more node created (the first on the new level)
       return $ Just ((l+1, n-1), ())	-- next level, n-1 more child at that level

-- Initialize new (sibling) node
-- treenode :: Monad m => Int -> Int -> TreeState Int (Int, Int) () m (Maybe ())
treenode _ n = do
  (l, r) <- getLevelState	-- remaining nodes in current level
  if r == 0
     then return Nothing	-- no further node here
     else do
       lift $ print $ "Level " ++ show l ++ " node " ++ show (n - r + 1)
       modifyTreeState $ \t -> t + 1		-- one more node here
       putLevelState (l, r-1)	-- level nodes go down
       return $ Just ()
