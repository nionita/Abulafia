{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             BangPatterns,
             RankNTypes, UndecidableInstances
             #-}

module Moves.BaseTypesD (
    CtxMon(..), Game
) where

import Control.Monad.IO.Class

import qualified Search.SearchMonadCPS as SM
import Struct.Struct
import Struct.Status
import Search.AlbetaTypesD

-- This is a specialized monad transformer for state
-- type Game m = SM.STPlus MyState m
type Game r m = SM.STPlus r MyState m

class (Monad m, MonadIO m) => CtxMon m where
    tellCtx :: Comm -> m ()
