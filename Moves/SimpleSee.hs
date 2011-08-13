module Moves.SimpleSEE
    -- (
    --     testSEE
    -- )
    where

import Data.Array.Unboxed
import Data.Bits

import qualified Struct.Struct as S
import Eval.BasicEval

-- We aim to simplify the SEE calculation for most probably situations and store them
-- (coded in the style of Ed Schröders lookup) in a precalculated array
-- We consider the maximum number of attackers per side and piece type (actually value)
-- as follows:
-- pawns:		maximal 2 -> 3 states -> 2 bits | (LSB)
-- minor pieces:	maximal 3 -> 4 states -> 2 bits |
-- rooks:		maximal 2 -> 3 states -> 2 bits |
-- queens:		maximal 1 -> 2 states -> 1 bit  |
-- king:		maximal 1 -> 2 states -> 1 bit  V (MSB)
-- which gives together 8 bits (per side)
-- we also need to consider the initial piece on the target square, which can be just one of
-- pawn, minor piece, rook or queen, giving altogether an array of [256][256][4] -> 256 K entries
-- The color of the target piece is always considered to be the opposite of the moving side
-- We ignore complications like x-ray, promotion and pinning
-- The entry size could be an integer (evtl. on 16 bits) for the SEE value; but having a fixed
-- assignement for the material (for example 100, 325, 500 and 975), the possible SEE values
-- are limited to + and - combinations of these values, and limited to minimal and maximal a queen
-- value (you cannot win or loose more than a queen in one exchange, unless you want it!)
-- So we could count an code the possible outcomes and use another lookup table for the real value
-- This has to be tuned to see what is better - trading space (chaches!) for speed

-- Implementantion of a simple SEE algorithm, where the "position" is the made from:
-- - initial piece on the target square
-- - configuration of the attackers
-- - configuration of the defenders

type PPiece = Int	-- we code the target piece type direct as the value

pawn  = matPiece S.White S.Pawn
minor = min (matPiece S.White S.Bishop) (matPiece S.White S.Knight)
rook  = matPiece S.White S.Rook
queen = matPiece S.White S.Queen
king  = matPiece S.White S.King

-- These codes are only intern used:
pawncode  = 0
minorcode = 1
rookcode  = 2
queencode = 3

data Actor = Actor { pawns, minors, rooks, queens, kings :: !Int }	-- attacker or defender "list"

data PPos = PPos PPiece Actor Actor		-- out pseudo position: target piece, attackers, defenders

-- The simple SEE recursive algorithm from chessprogramming.wikispaces.com, translated to Haskell:
seeValue :: PPos -> Int
seeValue (PPos piece attacks defends)
    | Just (npiece, ndefends) <- getSmallestAttacker attacks = max 0 $ piece - seeValue (PPos npiece defends ndefends)
    | otherwise                                              = 0

-- Choose the smallest attackers and return it, together with the updated attackers "list"
-- If there is no further attacker, return Nothing
getSmallestAttacker :: Actor -> Maybe (PPiece, Actor)
getSmallestAttacker act
    | pawns  act > 0 = Just (pawn,  act { pawns  = pawns  act - 1 })
    | minors act > 0 = Just (minor, act { minors = minors act - 1 })
    | rooks  act > 0 = Just (rook,  act { rooks  = rooks  act - 1 })
    | queens act > 0 = Just (queen, act { queens = queens act - 1 })
    | kings  act > 0 = Just (king,  act { kings  = kings  act - 1 })
    | otherwise      = Nothing

-- To precompute the big array of 256 K entries
-- This could be made lazy (and compute the values as they are needed) if we go the other way round,
-- from index to configuration (which is faster?)
seePrecompValues :: UArray Int Int
seePrecompValues = array (0, maxseeidx) [(idx, seeval) |
{--
                       (piece, base)   <- [(pawn, 0), (minor, 0x10000), (rook, 0x20000), (queen, 0x30000)],
                       (akings, idx1)  <- zip [0, 1] [base, base + 0x8000],
                       (aqueens, idx2) <- zip [0, 1] [idx1, idx1 + 0x4000],
                       (arooks, idx3)  <- zip [0 .. 2] [idx2, idx2 + 0x1000, idx2 + 0x2000],
                       (aminors, idx4) <- zip [0 .. 3] [idx3, idx3 + 0x400, idx3 + 0x800, idx3 + 0xC00],
                       (apawns, idx5)  <- zip [0 .. 2] [idx4, idx4 + 0x100, idx4 + 0x200],
                       let attacks = Actor apawns aminors arooks aqueens akings,
                       (dkings, idx6)  <- zip [0, 1] [idx5, idx5 + 0x80],
                       (dqueens, idx7) <- zip [0, 1] [idx6, idx6 + 0x40],
                       (drooks, idx8)  <- zip [0 .. 2] [idx7, idx7 + 0x10, idx7 + 0x20],
                       (dminors, idx9) <- zip [0 .. 3] [idx8, idx8 + 0x4, idx8 + 0x8, idx8 + 0xC],
                       (dpawns, idx)   <- zip [0 .. 2] [idx9, idx9 + 0x1, idx9 + 0x2],
--}
                       (piece, pcode)  <- [(pawn, 0), (minor, 1), (rook, 2), (queen, 3)],
                       akings  <- [0, 1],
                       aqueens <- [0, 1],
                       arooks  <- [0 .. 2],
                       aminors <- [0 .. 3],
                       apawns  <- [0 .. 2],
                       let attacks = Actor apawns aminors arooks aqueens akings,
                       dkings  <- [0, 1],
                       dqueens <- [0, 1],
                       drooks  <- [0 .. 2],
                       dminors <- [0 .. 3],
                       dpawns  <- [0 .. 2],
                       let defends = Actor dpawns dminors drooks dqueens dkings,
                       let idx = actToIdx pcode attacks defends,
                       let seeval = seeValue (PPos piece attacks defends)
                   ]
    where maxseeidx = 2 ^ 16 * 4

-- Read the SEE value from the pre computed array:
seeFromPreComp :: Int -> Actor -> Actor -> Int
seeFromPreComp pcode at de = seePrecompValues ! actToIdx pcode at de

actToIdx pcode (Actor apawns aminors arooks aqueens akings)
               (Actor dpawns dminors drooks dqueens dkings)
    = (min 3 pcode `shiftL` 16) .|. (aidx `shiftL` 8) .|. didx
    where didx =    min 2 dpawns
                .|. (min 3 dminors `shiftL`  2)
                .|. (min 2 drooks  `shiftL`  4)
                .|. (min 1 dqueens `shiftL`  6)
                .|. (min 1 dkings  `shiftL`  7)
          aidx =    min 2 apawns
                .|. (min 3 aminors `shiftL`  2)
                .|. (min 2 arooks  `shiftL`  4)
                .|. (min 1 aqueens `shiftL`  6)
                .|. (min 1 akings  `shiftL`  7)

hasAttacks :: Actor -> Bool
hasAttacks (Actor p m r q k) = p > 0 || m > 0 || r > 0 || q > 0 || k > 0

-- To compute the highest piece which can make a good capture, together with the value of this capture,
-- given a "position":
seeLimit :: PPos -> (Int, Int)
seeLimit (PPos piece attacks defends)
    | kings  attacks > 0 && kvalue >= 0 = (king,  kvalue)
    | queens attacks > 0 && qvalue >= 0 = (queen, qvalue)
    | rooks  attacks > 0 && rvalue >= 0 = (rook,  rvalue)
    | minors attacks > 0 && mvalue >= 0 = (minor, mvalue)
    | pawns  attacks > 0 && pvalue >= 0 = (pawn,  pvalue)
    | otherwise                         = (0, 0)
    where kvalue = if hasAttacks defends then (-king) else piece	-- we cannot capture with the king if it's defended
          qvalue = piece - seeFromPreComp queencode defends attacks { queens = queens attacks - 1 }
          rvalue = piece - seeFromPreComp rookcode  defends attacks { rooks  = rooks  attacks - 1 }
          mvalue = piece - seeFromPreComp minorcode defends attacks { minors = minors attacks - 1 }
          pvalue = piece - seeFromPreComp pawncode  defends attacks { pawns  = pawns  attacks - 1 }

-- For tests:
testSEE :: S.Piece -> [S.Piece] -> [S.Piece] -> (Int, [S.Piece])
testSEE piece lattacks ldefends = (val, wins)
    where (lim, val) = seeLimit $ PPos (pcode piece)
                                       (Actor apawns aminors arooks aqueens akings)
                                       (Actor dpawns dminors drooks dqueens dkings)
          apawns  = count S.Pawn lattacks
          aminors = count S.Bishop lattacks + count S.Knight lattacks
          arooks  = count S.Rook lattacks
          aqueens = count S.Queen lattacks
          akings  = count S.King lattacks
          dpawns  = count S.Pawn ldefends
          dminors = count S.Bishop ldefends + count S.Knight ldefends
          drooks  = count S.Rook ldefends
          dqueens = count S.Queen ldefends
          dkings  = count S.King ldefends
          wins = filter ((<= lim) . matPiece S.White) lattacks
          count p = length . filter (== p)
          pcode S.Queen  = queencode
          pcode S.Rook   = rookcode
          pcode S.Bishop = minorcode
          pcode S.Knight = minorcode
          pcode S.Pawn   = pawncode
