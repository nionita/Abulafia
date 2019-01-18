{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Search.GameTypes1 where

import Control.Monad

class Player a where
    type Opponent a :: *

-- Concrete types for our game
data Me
data You
data ChessGame

instance Player Me where
    type Opponent Me = You

instance Player You where
    type Opponent You = Me

-- Some other data types
data Path a	-- = ...

data NodeState a	-- = ...

negPath :: Player a => Path a -> Path (Opponent a)
negPath _ = undefined

-- Now we can define for example a function which establishes all
-- the needed relations between types
-- Here a and b are either Me and You or You and Me
-- and no other type combination is possible and the compiler knows that
search :: (Monad m, Player a) => Path a -> Path a -> NodeState (Opponent a) -> m (Path (Opponent a))
search a b nst = return $ negPath a
