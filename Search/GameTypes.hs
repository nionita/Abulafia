{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Search.GameTypes where

-- Define opponent relation between 2 types,
-- which determine a game type
class Opponent a b where
    type GameType a b :: *

-- The opponent relation is symetric
-- (same game type)
instance (Opponent a b, GameType a b ~ c) => Opponent b a where
    type GameType b a = c

-- Define an associated data type for game types
-- which delivers the 2 opponents
class HasActors g a b | g -> a, g -> b

-- Concrete types for our game
data Me
data You
data ChessGame

-- Me and You are opponents in the chess game
instance Opponent Me You where
    type GameType Me You = ChessGame

-- The actors of the chess game are Me and You
instance HasActors ChessGame Me You

-- Dummy class for a search monad
class Node m

-- Define a type class to put all restrictions together
class (Node m, Opponent a b, GameType a b ~ g, HasActors g a b) => Related a b g m

-- Some other data types
data Path a	-- = ...

data NodeState a	-- = ...

-- Now we can define for example a function which establishes all
-- the needed relations between types
-- Here a and b are either Me and You or You and Me
-- and no other type combination is possible and the compiler knows that
-- search :: Related a b ChessGame m => Path a -> Path a -> NodeState b -> Search m (Path b)
-- search ... = ...
