module Struct.Evolve where
import Control.Concurrent.Chan
-- import qualified Data.Vector.Unboxed as V

type Player = String

type Event  = String

data Result = ToPlay
            | Playing
            | Done (Int, Int, Int)
            deriving (Show, Read)

type ResReturn = ((Int, Int), Maybe (Int, Int, Int))

data Pairing = Pairing {
                  pair   :: (Int, Int),
                  result :: Result
               } deriving (Show, Read)

-- type Vec = V.Vector Double	-- Vector in compact form
type Vec = [Double]		-- Vector in list form
type Distrib = (Int, (Vec, Vec))	-- distribution

data Tournament
    = Tournament {
        event   :: Event,
        players :: [Player],
        games   :: [Pairing]
      } deriving (Show, Read)

data Phase = Initialize | Prepare | Play deriving (Show, Read)

data EvolvePersistentState
    = Pers {
        evName     :: String,		-- evolve name
        evPopCount :: Int,		-- population count (without witness)
        evPhase    :: Phase,		-- current phase
        evDistrib  :: Distrib,		-- current distribution
        evCycle    :: Int,		-- current cycle (number of tournaments begun)
        evPParams  :: [(Player, Vec)],	-- current player parameters
        evActSucc  :: [(Player, Rational)],	-- points of active players
        evCurTour  :: Maybe Tournament,	-- current (or last) tournament
        evWitness  :: Maybe Player,	-- witness player
        evWitSucc  :: [Rational]	-- points of the witness over time (reverse)
      } deriving (Show, Read)

data EvolveState
    = EvSt {
        stPers     :: EvolvePersistentState,	-- the persistent state
        stChan     :: Chan ResReturn,		-- channel for game result returns
        stMaxThr   :: Int,			-- maximum number of running games
        stCurThr   :: Int 			-- current number of running games
      }
