{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Search.AlbetaTypes (
    Node(..),
    Edge(..),
    Score(..),
    DoResult(..),
    Comm(..),
    ABControl(..)
) where

import Control.Monad
-- import Data.List (delete, sortBy)
-- import Data.Ord (comparing)
-- import Data.Array.Base
-- import Data.Array.Unboxed

import Search.SearchMonad

class (Ord s, Num s, Bounded s) => Score s where
    nextlev  :: s -> s
    nearmate :: s -> Bool
    toInt    :: s -> Int
    fromInt  :: Int -> s

class Edge e where
    special :: e -> Bool

data ABControl e s = ABC {
        maxdepth :: !Int,
        lastpv :: [e],
        lastscore :: Maybe s,
        rootmvs :: [e],
        window :: s,
        learnev :: Bool,
        best :: Bool
    } deriving Show

-- The node class, dependent on a game monad m, an edge type e (moves)
-- and a score type s
class (Monad m, Eq e, Show e, Edge e, Score s, Show s) =>
  Node m e s | m -> e, m -> s, s -> m where
    staticVal :: m s  -- static evaluation of a node
    materVal  :: m s  -- material evaluation (for prune purpose)
    genEdges :: Int -> Int -> Bool -> m ([e], [e])  -- generate all legal edges
    genTactEdges :: m [e]  -- generate all edges in tactical positions
    legalEdge :: e -> m Bool	-- is the move legal?
    killCandEdge :: e -> e -> m Bool	-- is the move killer candidate?
    inSeq :: e -> e -> m Bool	-- can 2 moves be in sequence?
    tactical :: m Bool -- if a position is tactical, search further
    doEdge   :: e -> Bool -> m (DoResult s)
    undoEdge :: e -> m ()
    betaMove :: Bool -> Int -> Int -> e -> m ()   -- called for beta-cut moves
    nullEdge :: m ()		   -- do null move (and also undo)
    retrieve :: m (Int, Int, s, e, Int)   -- retrieve the position in hash
    store :: Int -> Int -> s -> e -> Int -> m () -- store the position in hash
    learn :: Int -> Int -> s -> s -> m ()	-- learn the evaluation parameters
    curNodes :: m Int
    inform :: Comm e s -> m ()		-- communicate to the world (log, current and best move)
    choose :: Bool -> [(s, [e])] -> m (s, [e])

data DoResult s = Exten !Int	-- return mit extension (evtl 0)
                | Final !s	-- return with a final score (probably draw)

data Comm e s = LogMes String
              | BestMv s Int Int [e]
              | CurrMv e Int
              | InfoStr String
