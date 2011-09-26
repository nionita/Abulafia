{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}

module Search.SearchMonadCPS (
    STPlus,
    return, (>>=),
    get, put, gets, modify,
    lift, liftIO,
    runSearch, execSearch
    ) where

import Control.Monad
import Control.Monad.State hiding (lift, gets, modify)

newtype STPlus r s m a = STPlus { runSTPlus :: s -> (a -> s -> m r) -> m r }
-- {-# INLINE runSTPlus #-}

-- newtype ContState r s a = ContState { runCS :: s -> (a -> s -> r) -> r }

instance Monad (STPlus r s m) where
    return a = STPlus $ \s k -> k a s
    c >>= f  = STPlus $ \s0 k -> runSTPlus c s0 $ \a s1 -> runSTPlus (f a) s1 k

instance MonadState s (STPlus r s m) where
    get   = STPlus $ \s k -> k s  s
    put s = STPlus $ \_ k -> k () s

{--
instance Monad m => Monad (STPlus s m) where
    {-# INLINE return #-}
    return v = STPlus (\s -> return (v, s))
    {-# INLINE (>>=) #-}
    (>>=)    = bindSTPlus

{-# INLINE bindSTPlus #-}
bindSTPlus :: Monad m => STPlus s m a -> (a -> STPlus s m b) -> STPlus s m b
-- bindSTPlus ms f = STPlus $ \s -> runSTPlus ms s >>= \(v', s') -> runSTPlus (f v') s'
bindSTPlus ms f = STPlus $ \s -> case runSTPlus ms s of
                                     m -> m >>= \(v', s') -> case f v' of
                                                                fv -> runSTPlus fv s'

instance Monad m => MonadState s (STPlus s m) where
    {-# INLINE get #-}
    get   = STPlus $ \s -> return (s,  s)
    {-# INLINE put #-}
    put s = STPlus $ \_ -> return ((), s)
--}

instance MonadIO m => MonadIO (STPlus r s m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

runSearch :: Monad m => STPlus (a, s) s m a -> s -> m (a, s)
runSearch c s = runSTPlus c s $ \a s0 -> return (a, s0)

execSearch ms s = liftM snd $ runSearch ms s

{-# INLINE lift #-}
lift :: Monad m => m a -> STPlus r s m a
lift m = STPlus $ \s k -> m >>= \a -> k a s

{-# INLINE gets #-}
gets :: Monad m => (s -> a) -> STPlus r s m a
-- gets f = STPlus $ \s k -> case f s of fs -> k fs s
gets f = STPlus $ \s k -> k (f s) s

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> STPlus r s m ()
-- modify f = STPlus $ \s k -> case f s of fs -> k () fs
modify f = STPlus $ \s k -> k () (f s)
