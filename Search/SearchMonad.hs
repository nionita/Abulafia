{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FlexibleInstances #-}

module Search.SearchMonad (
    STPlus,
    return, (>>=),
    get, put, gets, modify,
    lift, liftIO,
    runSearch, execSearch
    ) where

import Control.Monad
import Control.Monad.State hiding (lift, gets, modify)
-- import Control.Monad.Cont

newtype STPlus s m a = STPlus { runSTPlus :: s -> m (a, s) }
{-# INLINE runSTPlus #-}

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

instance MonadIO m => MonadIO (STPlus s m) where
    {-# INLINE liftIO #-}
    liftIO = lift . liftIO

runSearch :: Monad m => STPlus s m a -> s -> m (a, s)
runSearch = runSTPlus

execSearch ms s = liftM snd $ runSearch ms s

{-# INLINE lift #-}
lift :: Monad m => m a -> STPlus s m a
lift m = STPlus $ \s -> m >>= \v -> return (v, s)

{-# INLINE gets #-}
gets :: Monad m => (s -> a) -> STPlus s m a
-- gets f = STPlus $ \s -> return (f s, s)
gets f = STPlus $ \s -> case f s of fs -> return (fs, s)

{-# INLINE modify #-}
modify :: Monad m => (s -> s) -> STPlus s m ()
modify f = STPlus $ \s -> case f s of fs -> return ((), fs)

{--
newtype CPST m s a = CPST
    { unCPST :: forall r. Cont (s -> (m a, s)) a }

instance Monad m => Monad (CPST m s) where
    return v     = CPST (\k -> k v)
    CPST a >>= f = CPST (\k -> a (\v -> unCPST (f v) k))

--}
-- type Search m e s = CPST m (PVState e s)
