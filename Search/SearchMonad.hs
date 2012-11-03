{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Search.SearchMonad (
    STPlus,
    -- return, (>>=),
    -- get, put,
    gets, modify,
    -- lift,
    -- liftIO,
    runSearch, execSearch
    ) where

import Control.Monad
import Control.Monad.State hiding (gets, modify)

newtype STPlus r s m a = STPlus { runSTPlus :: s -> (a -> s -> m r) -> m r }
-- {-# INLINE runSTPlus #-}

instance Monad (STPlus r s m) where
    return a = STPlus $ \s k -> k a s
    {-# INLINE return #-}
    c >>= f  = STPlus $ \s0 k -> runSTPlus c s0 $ \a s1 -> case f a of !fa -> runSTPlus fa s1 k
    -- c >>= f  = STPlus $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (f a) s1 k
    {-# INLINE (>>=) #-}

instance MonadState s (STPlus r s m) where
    get   = STPlus $ \s k -> k s  s
    {-# INLINE get #-}
    put s = STPlus $ \_ k -> k () s
    {-# INLINE put #-}

instance MonadTrans (STPlus r s) where
    {-# INLINE lift #-}
    -- lift :: Monad m => m a -> STPlus r s m a
    lift m = STPlus $ \s k -> m >>= \a -> k a s


instance MonadIO m => MonadIO (STPlus r s m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

runSearch :: Monad m => STPlus (a, s) s m a -> s -> m (a, s)
runSearch c s = runSTPlus c s $ \a s0 -> return (a, s0)
{-# INLINE runSearch #-}

execSearch ms s = liftM snd $ runSearch ms s
{-# INLINE execSearch #-}

{-# INLINE gets #-}
gets :: Monad m => (s -> a) -> STPlus r s m a
gets f = STPlus $ \s k -> case f s of !fs -> k fs s
-- gets f = STPlus $ \s k -> k (f s) s

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> STPlus r s m ()
modify f = STPlus $ \s k -> case f s of !fs -> k () fs
-- modify f = STPlus $ \s k -> k () (f s)
