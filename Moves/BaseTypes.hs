{-# LANGUAGE TypeSynonymInstances,
             MultiParamTypeClasses,
             BangPatterns,
             RankNTypes, UndecidableInstances
             #-}

module Moves.BaseTypes (
    CtxMon(..), Game
) where

import Control.Monad.IO.Class
-- import Data.Array.IArray
-- import Data.Array.Unboxed
-- import Debug.Trace
-- import Control.Exception (assert)
-- import Data.Word
-- import Data.Bits
-- import Data.List
-- import Data.Char
-- import Data.Maybe
-- import Control.Monad.State.Lazy
-- import Control.Monad.Reader
-- import Data.Ord (comparing)
-- import Data.Array.IO
-- import System.Random

import qualified Search.SearchMonad as SM
import Struct.Struct
import Struct.Status
import Search.AlbetaTypes

-- This is a specialized monad transformer for state
-- type Game m = SM.STPlus MyState m
type Game = SM.STPlus MyState

class (Monad m, MonadIO m) => CtxMon m where
    tellCtx :: Comm Move Int -> m ()

{--
instance Score Int where
    {-# INLINE nextlev #-}
    nextlev !i
        | i >= mateScore - 255    = -im
        | i <= (-mateScore) + 255 = -ip
        | otherwise               = -i
        where !ip = i + 1
              !im = i - 1
    nearmate i = i >= mateScore - 255 || i <= -mateScore + 255

instance Edge Move where
    {-# INLINE special #-}
    special = moveIsSpecial
--}
