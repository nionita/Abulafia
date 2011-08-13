module Struct.Evolve where
-- import qualified Data.Vector.Unboxed as V

type Player = String
type Event  = String
type Result = Maybe (Int, Int, Int)

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

data EvolveStatus
    = Evs {
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
