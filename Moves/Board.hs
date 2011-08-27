{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, PatternGuards, BangPatterns #-}
module Moves.Board (
    posFromFen, initPos,
    isCheck, inCheck,
    goPromo, hasMoves,
    genmv, genmvT,
    genMoveCapt, genMoveCast, genMoveNCapt, genMoveTransf, genMovePCapt, genMovePNCapt, genMoveFCheck,
    genMoveNCaptToCheck,
    updatePos, illegalPos,
    legalMove, alternateMoves, nonCapt,
    doFromToMove, reverseMoving
    ) where

import Prelude hiding ((++), foldl, filter, map, concatMap, concat, head, tail, repeat, zip,
                       zipWith, null, words, foldr, elem, lookup, any)
import Control.Exception (assert)
import Data.Array.IArray
import Data.Bits
import Data.List.Stream
import Data.Char
import Data.Maybe
import Data.Ord (comparing)

import Struct.Struct
import Moves.Moves
import Moves.BitBoard
import Moves.ShowMe
import Moves.SEE
import Eval.BasicEval
import Hash.Zobrist

startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR/ w KQkq - 0 1"

fenToTable :: String -> MyPos
fenToTable fen = foldr setp emptyPos $ fenToAssocs fen
    where setp (sq, (c, p)) = setPiece sq c p

fenToAssocs :: String -> [(Square, (Color, Piece))]
fenToAssocs str = go 56 str []
    where go _ [] acc = acc
          go sq (c:cs) acc
              | c `elem` "PRNBQK" = go (sq+1) cs $ (sq, fcw):acc
              | c `elem` "prnbqk" = go (sq+1) cs $ (sq, fcb):acc
              | c == '/'  = go (nextline sq) cs acc
              | isDigit c = go (skip sq c) cs acc
              | otherwise = go sq cs acc	-- silently ignore other chars
              where fcw = (White, toPiece c)
                    fcb = (Black, toPiece $ toUpper c)
          skip f c = f + fromIntegral (ord c - ord '0')
          nextline f = f - 16
          toPiece c = fromJust $ lookup c letterToPiece

letterToPiece = [('P', Pawn), ('R', Rook), ('N', Knight), ('B', Bishop),
                    ('Q', Queen), ('K', King)]

initPos = posFromFen startFen

-- TODO: en passant
posFromFen :: String -> MyPos
posFromFen fen = p { basicPos = bp, zobkey = zk }
    where fen1:fen2:fen3:fen4:fen5:_ = fenFromString fen
          p  = fenToTable fen1
          bp = (basicPos p) { bpepcas = x }
          x  = fyInit . castInit . epInit $ epcas0
          (epcas0, z) = case fen2 of
              'w':_ -> (0, 0)
              'b':_ -> (mvMask, zobMove)
          (cK, z1) = if 'K' `elem` fen3 then ((.|. caRKiw), zobCastKw) else (id, 0)
          (cQ, z2) = if 'Q' `elem` fen3 then ((.|. caRQuw), zobCastQw) else (id, 0)
          (ck, z3) = if 'k' `elem` fen3 then ((.|. caRKib), zobCastKb) else (id, 0)
          (cq, z4) = if 'q' `elem` fen3 then ((.|. caRQub), zobCastQb) else (id, 0)
          castInit = cQ . cK . cq . ck
          epInit   = id					-- TODO: ep field
          fyInit = set50Moves $ read fen5
          zk = zobkey p `xor` z `xor` z1 `xor` z2 `xor` z3 `xor` z4		-- also for ep

-- A primitive decomposition of the fen string
fenFromString :: String -> [String]
fenFromString fen = zipWith ($) fenfuncs fentails
    where fentails = tails $ words fen
          fenfuncs = [ getFenPos, getFenMv, getFenCast, getFenEp, getFenHalf, getFenMvNo ]
          headOrDefault a0 as = if null as then a0 else head as
          getFenPos  = headOrDefault ""
          getFenMv   = headOrDefault "w"
          getFenCast = headOrDefault "-"
          getFenEp   = headOrDefault "-"
          getFenHalf = headOrDefault "-"
          getFenMvNo = headOrDefault "-"

{--
-- find rook-like possibly pinning pieces for a position & color
-- that is: rooks or queens, which possibly pin oponent pieces in regard to the (oponent) king
findPKAr p c = rAttacs defksq 0 .&. rs .&. atp
    where (atp, defp) = if c == White then (white p, black p) else (black p, white p)
          rs = rooks p .|. queens p
          defksq = firstOne $ defp .&. kings p

-- find bishop-like possibly pinning pieces for a position & color
-- that is: bishops or queens, which possibly pin oponent pieces in regard to the (oponent) king
findPKAb p c = bAttacs defksq 0 .&. bs .&. atp
    where (atp, defp) = if c == White then (white p, black p) else (black p, white p)
          bs = bishops p .|. queens p
          defksq = firstOne $ defp .&. kings p

-- find all possibly pining pieces and lines in a given position
-- this has to be calculated per position, and recalculated
-- only when the king or one of the pinning pieces move or is captured
allPLKAs p = (lwr ++ lwb, lbr ++ lbb)
    where pkaswr = findPKAr p White
          pkaswb = findPKAb p White
          pkasbr = findPKAr p Black
          pkasbb = findPKAb p Black
          kwsq = firstOne $ kings p .&. white p
          kbsq = firstOne $ kings p .&. black p
          lwr = filter f $ map (findLKA Rook kbsq) $ bbToSquares pkaswr
          lwb = filter f $ map (findLKA Bishop kbsq) $ bbToSquares pkaswb
          lbr = filter f $ map (findLKA Rook kwsq) $ bbToSquares pkasbr
          lbb = filter f $ map (findLKA Bishop kwsq) $ bbToSquares pkasbb
          f = (/= 0) . snd
--}

-- For pinned pieces the move generation is restricted to the pinned line
-- so the same attacs .&. direction
pinningDir :: MyPos -> Color -> Square -> BBoard
pinningDir p c sq = let ds = filter (exactOne . (.&. bit sq)) $ map snd
                                $ if c == White then bpindirs p else wpindirs p
                    in if null ds then error "pinningDir" else head ds

pinningCapt :: MyPos -> Color -> Square -> BBoard
pinningCapt p c sq = let ds = filter (exactOne . (.&. bit sq) . snd)
                                  $ if c == White then bpindirs p else wpindirs p
                     in if null ds then error "pinningCapt" else bit . fst . head $ ds

-- Is color c in check in position p?
isCheck :: MyPos -> Color -> Bool
isCheck p c = (ckp /= 0) && (ckp .&. colp /= 0)
    where colp = if c == White then white p else black p
          ckp = check p

-- {-# INLINE inCheck #-}
inCheck :: MyPos -> Bool
inCheck p = moving p == White && isCheck p White
            || moving p == Black && isCheck p Black

goPromo :: MyPos -> Move -> Bool
goPromo p m
    | moveIsTransf m = True
    | otherwise      = case tabla p t of
                           Busy White Pawn -> ppw
                           Busy Black Pawn -> ppb
                           _               -> False
    where !t = toSquare m
          !ppw = t >= 48	-- 40
          !ppb = t < 16		-- 24

-- {-# INLINE genmv #-}
genmv :: Bool -> MyPos -> (Square, Square) -> Move
genmv spec p (f, t) = if spec then makeSpecial m else m
    where !m = moveFromTo f t

-- Used only with transformation pawns
genmvT :: MyPos -> (Square, Square) -> Move
genmvT p (f, t) = makeTransf Queen f t

hasMoves :: MyPos -> Color -> Bool
hasMoves p c = (check && (not . null $ genMoveFCheck p c)) || (not check && anyMove)
    where hasPc = any (/= 0) $ map (pcapt . flip pAttacs c)
                     $ bbToSquares $ pawns p .&. myfpc
          hasPm = not . null $ pAll1Moves c (pawns p .&. mypc) (occup p)
          hasN = any (/= 0) $ map (legmv . nAttacs) $ bbToSquares $ knights p .&. myfpc
          hasB = any (/= 0) $ map (legmv . flip bAttacs (occup p))
                     $ bbToSquares $ bishops p .&. myfpc
          hasR = any (/= 0) $ map (legmv . flip rAttacs (occup p))
                     $ bbToSquares $ rooks p .&. myfpc
          hasQ = any (/= 0) $ map (legmv . flip qAttacs (occup p))
                     $ bbToSquares $ queens p .&. myfpc
          hasK = 0 /= (legal . kAttacs $ firstOne $ kings p .&. mypc)
          anyMove = hasK || hasN || hasPm || hasPc || hasQ || hasR || hasB
          !check = inCheck p
          (mypc, yopi) = thePieces p c
          myfpc = mypc `less` pinned p
          yopiep = yopi .|. (epcas p .&. epMask)
          legmv x = x `less` mypc
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          !oppAt = if c == White then blAttacs p else whAttacs p

-- Move generation generates legal moves
genMoveCapt :: MyPos -> Color -> [(Square, Square)]
genMoveCapt p c = sortByMVVLVA p all
-- genMoveCapt p c = sortByMatDiff p all
    where !pGenC = concatMap (srcDests (pcapt . flip pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc `less` traR
          !nGenC = concatMap (srcDests (capt . nAttacs)) 
                     $ bbToSquares $ knights p .&. myfpc
          !bGenC = concatMap (srcDests (capt . flip bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. myfpc
          !rGenC = concatMap (srcDests (capt . flip rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. myfpc
          !qGenC = concatMap (srcDests (capt . flip qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. myfpc
          !kGenC =            srcDests (capt . legal . kAttacs)
                     $ firstOne $ kings p .&. myfpc
          all  = concat [ pGenC, nGenC, bGenC, rGenC, qGenC, kGenC ]
          (mypc, yopi) = thePieces p c
          myfpc = mypc `less` pinned p
          -- yopi  = yoPieces p c
          yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          !oppAt = if c == White then blAttacs p else whAttacs p
          !traR = if c == White then 0x00FF000000000000 else 0xFF00

-- For quiescent search we generate only winning captures
-- This is just an approximation
genMoveWCapt :: MyPos -> Color -> [(Square, Square)]
genMoveWCapt !p !c = concat [ pGenC, nGenC, bGenC, rGenC, qGenC, kGenC ]
    where pGenC = concatMap (srcDests (pcapt . flip pAttacs c))
                     $ bbToSquares $ pawns p .&. mypc `less` traR
          nGenC = concatMap (srcDests (wcapt yopfornb . nAttacs)) 
                     $ bbToSquares $ knights p .&. mypc
          bGenC = concatMap (srcDests (wcapt yopfornb . flip bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc
          rGenC = concatMap (srcDests (wcapt yopforr . flip rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc
          qGenC = concatMap (srcDests (wcapt yopforq . flip qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc
          kGenC =            srcDests (capt . legal . kAttacs)
                     $ firstOne $ kings p .&. mypc
          mypc = myPieces p c `less` pinned p
          yopi  = yoPieces p c
          yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          wcapt y x = x .&. y
          pcapt x = x .&. yopiep
          legal x = x `less` oppAt
          oppAt = if c == White then blAttacs p else whAttacs p
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          hanging  = yopi `less` oppAt
          yopfornb = hanging .|. (yopi `less` pawns p)
          yopforr  = hanging .|. (yopfornb `less` knights p `less` bishops p)
          yopforq  = hanging .|. (yopi .&. queens p)

genMoveNCapt :: MyPos -> Color -> [(Square, Square)]
-- genMoveNCapt p c = concat [ pGenNC2, qGenNC, rGenNC, bGenNC, nGenNC, pGenNC1, kGenNC ]
-- genMoveNCapt p c = concat [ pGenNC1, nGenNC, bGenNC, rGenNC, qGenNC, pGenNC2, kGenNC ]
genMoveNCapt p c = concat [ nGenNC, bGenNC, rGenNC, qGenNC, pGenNC1, pGenNC2, kGenNC ]
    -- where pGenNCT = concatMap (srcDests True (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. mypc .&. traR
    --       pGenNC = concatMap (srcDests False (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. mypc `less` traR
    where pGenNC1 = pAll1Moves c (pawns p .&. mypc `less` traR) (occup p)
          pGenNC2 = pAll2Moves c (pawns p .&. mypc) (occup p)
          nGenNC = concatMap (srcDests (ncapt . nAttacs))
                      $ bbToSquares $ knights p .&. mypc
          bGenNC = concatMap (srcDests (ncapt . flip bAttacs (occup p)))
                      $ bbToSquares $ bishops p .&. mypc
          rGenNC = concatMap (srcDests (ncapt . flip rAttacs (occup p)))
                      $ bbToSquares $ rooks p .&. mypc
          qGenNC = concatMap (srcDests (ncapt . flip qAttacs (occup p)))
                      $ bbToSquares $ queens p .&. mypc
          kGenNC =            srcDests (ncapt . legal . kAttacs)
                      $ firstOne $ kings p .&. mypc
          mypc = myPieces p c `less` pinned p
          ncapt x = x `less` occup p
          legal x = x `less` oppAt
          oppAt = if c == White then blAttacs p else whAttacs p
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          mypawns = pawns p .&. mypc

-- Generate only transformations (now only to queen) - captures and non captures
genMoveTransf :: MyPos -> Color -> [(Square, Square)]
genMoveTransf p c = pGenC ++ pGenNC
    where pGenC = concatMap (srcDests (pcapt . flip pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc
    --       pGenNC = concatMap (srcDests False (ncapt . \s -> pMovs s c ocp)) 
    --                  $ bbToSquares $ pawns p .&. myfpc .&. traR
          pGenNC = pAll1Moves c (pawns p .&. myfpc) (occup p)
          (mypc, yopi) = thePieces p c
          myfpc = mypc .&. traR `less` pinned p
          -- yopi  = yoPieces p c
          yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          pcapt x = x .&. yopiep
          traR = if c == White then 0x00FF000000000000 else 0xFF00

-- Generate the captures with pinned pieces
genMovePCapt :: MyPos -> Color -> [(Square, Square)]
genMovePCapt !p !c = concat [ pGenC, nGenC, bGenC, rGenC, qGenC ]
    where pGenC = concatMap (srcDests $ pinCapt p c (pcapt . flip pAttacs c))
                     $ bbToSquares $ pawns p .&. myfpc `less` traR
          nGenC = concatMap (srcDests $ pinCapt p c (capt . nAttacs)) 
                     $ bbToSquares $ knights p .&. myfpc
          bGenC = concatMap (srcDests $ pinCapt p c (capt . flip bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. myfpc
          rGenC = concatMap (srcDests $ pinCapt p c (capt . flip rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. myfpc
          qGenC = concatMap (srcDests $ pinCapt p c (capt . flip qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. myfpc
          (mypc, yopi) = thePieces p c
          myfpc = mypc .&. pinned p
          -- yopi  = yoPieces p c
          yopiep = yopi .|. (epcas p .&. epMask)
          capt x = x .&. yopi
          pcapt x = x .&. yopiep
          traR = if c == White then 0x00FF000000000000 else 0xFF00

-- Generate the non-captures with pinned pieces
genMovePNCapt :: MyPos -> Color -> [(Square, Square)]
genMovePNCapt !p !c = concat [ pGenNC, qGenNC, rGenNC, bGenNC, nGenNC ]
    where pGenNC = concatMap (srcDests $ pinMove p c (ncapt . \s -> pMovs s c (occup p))) 
                     $ bbToSquares $ pawns p .&. mypc `less` traR
          nGenNC = concatMap (srcDests $ pinMove p c (ncapt . nAttacs))
                      $ bbToSquares $ knights p .&. mypc
          bGenNC = concatMap (srcDests $ pinMove p c (ncapt . flip bAttacs (occup p)))
                      $ bbToSquares $ bishops p .&. mypc
          rGenNC = concatMap (srcDests $ pinMove p c (ncapt . flip rAttacs (occup p)))
                      $ bbToSquares $ rooks p .&. mypc
          qGenNC = concatMap (srcDests $ pinMove p c (ncapt . flip qAttacs (occup p)))
                      $ bbToSquares $ queens p .&. mypc
          mypc = myPieces p c .&. pinned p
          ncapt x = x `less` occup p
          traR = if c == White then 0x00FF000000000000 else 0xFF00
          mypawns = pawns p .&. mypc

-- {-# INLINE pinMove #-}
pinMove :: MyPos -> Color -> (Square -> BBoard) -> Square -> BBoard
pinMove p c f sq = f sq .&. pinningDir p c sq

-- {-# INLINE pinCapt #-}
pinCapt :: MyPos -> Color -> (Square -> BBoard) -> Square -> BBoard
pinCapt p c f sq = f sq .&. pinningCapt p c sq

-- {-# INLINE srcDests #-}
srcDests :: (Square -> BBoard) -> Square -> [(Square, Square)]
srcDests f !s = zip (repeat s) $ bbToSquares $ f s

-- Because finding the blocking square for a queen check is so hard,
-- we define a data type and, in case of a queen check, we give also
-- the piece type (rook or bishop) in which direction the queen checks
data CheckInfo = NormalCheck Piece !Square
               | QueenCheck Piece !Square

-- Finds pieces which check
findChecking :: MyPos -> Color -> [CheckInfo]
findChecking !p !c = concat [ pChk, nChk, bChk, rChk, qbChk, qrChk ]
    where pChk = map (NormalCheck Pawn) $ filter ((/= 0) . kattac . flip pAttacs c)
                               $ bbToSquares $ pawns p .&. mypc
          nChk = map (NormalCheck Knight) $ filter ((/= 0) . kattac . nAttacs)
                               $ bbToSquares $ knights p .&. mypc
          bChk = map (NormalCheck Bishop) $ filter ((/= 0) . kattac . flip bAttacs (occup p))
                               $ bbToSquares $ bishops p .&. mypc
          rChk = map (NormalCheck Rook) $ filter ((/= 0) . kattac . flip rAttacs (occup p))
                               $ bbToSquares $ rooks p .&. mypc
          qbChk = map (QueenCheck Bishop) $ filter ((/= 0) . kattac . flip bAttacs (occup p))
                               $ bbToSquares $ queens p .&. mypc
          qrChk = map (QueenCheck Rook) $ filter ((/= 0) . kattac . flip rAttacs (occup p))
                               $ bbToSquares $ queens p .&. mypc
          -- mypc = myPieces p c
          -- yopi  = yoPieces p c
          (!mypc, !yopi) = thePieces p c
          kattac x = x .&. kings p .&. yopi

-- Generate move when in check
genMoveFCheck :: MyPos -> Color -> [(Square, Square)]
genMoveFCheck p c
    | null chklist = error "genMoveFCheck"
    | null $ tail chklist = r1 ++ kGen ++ r2	-- simple check
    | otherwise = kGen				-- double check, only king moves help
    where chklist = findChecking p $ other c
          !kGen = srcDests (legal . kAttacs) ksq
          !ksq = firstOne kbb
          !kbb = kings p .&. mypc
          !ocp1 = occup p `less` kbb
          legal x = x `less` alle
          !alle = mypc .|. oppAt .|. excl
          !mypc = myPieces p c
          !oppAt = if c == White then blAttacs p else whAttacs p
          !excl = foldl' (.|.) 0 $ map chkAtt chklist
          chkAtt (NormalCheck f s) = fAttacs s f ocp1
          chkAtt (QueenCheck f s)  = fAttacs s f ocp1
          (r1, r2) = case head chklist of	-- this is needed only when simple check
                 NormalCheck Pawn sq   -> (beatAt p c (bit sq), [])  -- cannot block pawn
                 NormalCheck Knight sq -> (beatAt p c (bit sq), [])  -- or knight check
                 NormalCheck Bishop sq -> beatOrBlock Bishop p c sq
                 NormalCheck Rook sq   -> beatOrBlock Rook p c sq
                 QueenCheck pt sq      -> beatOrBlock pt p c sq

-- Generate moves ending on a given square (used to defend a check by capture or blocking)
-- This part is only for queens, rooks, bishops and knights (no pawns and, of course, no kings)
defendAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
defendAt p c bb = concat [ nGenC, bGenC, rGenC, qGenC ]
    where nGenC = concatMap (srcDests (target . nAttacs))
                     $ bbToSquares $ knights p .&. mypc `less` pinned p
          bGenC = concatMap (srcDests (target . flip bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc `less` pinned p
          rGenC = concatMap (srcDests (target . flip rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc `less` pinned p
          qGenC = concatMap (srcDests (target . flip qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc `less` pinned p
          target x = x .&. bb
          mypc = myPieces p c

-- Generate capture pawn moves ending on a given square (used to defend a check by capture)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBeatAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
pawnBeatAt p c bb = concatMap (srcDests (pcapt . flip pAttacs c))
                           $ bbToSquares $ pawns p .&. mypc `less` pinned p
    where -- yopi  = yoPieces p c
          yopiep = bb .&. (yopi .|. (epcas p .&. epMask))
          pcapt x = x .&. yopiep
          -- mypc = myPieces p c
          (mypc, yopi) = thePieces p c

-- Generate blocking pawn moves ending on given squares (used to defend a check by blocking)
-- TODO: Here: the promotion is not correct (does not promote!)
pawnBlockAt :: MyPos -> Color -> BBoard -> [(Square, Square)]
pawnBlockAt p c bb = concatMap (srcDests (block . \s -> pMovs s c (occup p))) 
                            $ bbToSquares $ pawns p .&. mypc `less` pinned p
    where block x = x .&. bb
          mypc = myPieces p c

beatAt p c bb = pawnBeatAt p c bb ++ defendAt p c bb

blockAt p c bb = pawnBlockAt p c bb ++ defendAt p c bb

-- Defend a check from a sliding piece: beat it or block it
beatOrBlock :: Piece -> MyPos -> Color -> Square -> ([(Square, Square)], [(Square, Square)])
beatOrBlock f p c sq = (beat, block)
    where !beat = beatAt p c $ bit sq
          atp = if c == White then white p else black p
          aksq = firstOne $ atp .&. kings p
          (_, line) = findLKA f aksq sq
          !block = blockAt p c line

genMoveNCaptToCheck :: MyPos -> Color -> [(Square, Square)]
genMoveNCaptToCheck p c = genMoveNCaptDirCheck p c ++ genMoveNCaptIndirCheck p c

-- Todo: check with pawns (should be also without transformations)
genMoveNCaptDirCheck :: MyPos -> Color -> [(Square, Square)]
-- genMoveNCaptDirCheck p c = concat [ nGenC, bGenC, rGenC, qGenC ]
genMoveNCaptDirCheck p c = concat [ qGenC, rGenC, bGenC, nGenC ]
    where nGenC = concatMap (srcDests (target nTar . nAttacs))
                     $ bbToSquares $ knights p .&. mypc `less` pinned p
          bGenC = concatMap (srcDests (target bTar . flip bAttacs (occup p)))
                     $ bbToSquares $ bishops p .&. mypc `less` pinned p
          rGenC = concatMap (srcDests (target rTar . flip rAttacs (occup p)))
                     $ bbToSquares $ rooks p .&. mypc `less` pinned p
          qGenC = concatMap (srcDests (target qTar . flip qAttacs (occup p)))
                     $ bbToSquares $ queens p .&. mypc `less` pinned p
          target b x = x .&. b
          (mypc, yopc) = thePieces p c
          ksq  = firstOne $ yopc .&. kings p
          nTar = fAttacs ksq Knight (occup p) `less` yopc
          bTar = fAttacs ksq Bishop (occup p) `less` yopc
          rTar = fAttacs ksq Rook   (occup p) `less` yopc
          qTar = bTar .|. rTar

-- TODO: indirect non capture checking moves
genMoveNCaptIndirCheck :: MyPos -> Color -> [(Square, Square)]
genMoveNCaptIndirCheck _ _ = []

sortByMatDiff :: MyPos -> [(Square, Square)] -> [(Square, Square)]
sortByMatDiff p = map snd . sortBy (comparing fst) . map diffs
    where diffs ft@(f, t) | Busy _ f1 <- tabla p f, Busy _ f2 <- tabla p t
                          = (matPiece White f1 - matPiece White f2, ft)
          --- Trick here, we need just order, not white/black score
          --- We make the difference so that sort deliveres the best captures first

sortByMVVLVA :: MyPos -> [(Square, Square)] -> [(Square, Square)]
sortByMVVLVA p = map snd . sortBy (comparing fst) . map va
    where va ft@(f, t) | Busy _ f1 <- tabla p f, Busy _ f2 <- tabla p t
                       = ((- matPiece White f2, matPiece White f1), ft)

-- {-# INLINE updatePos #-}
updatePos :: Bool -> MyPos -> MyPos
updatePos recalc = updatePosCheck recalc . updatePosAttacs . updatePosOccup

updatePosOccup :: MyPos -> MyPos
updatePosOccup p = p {
                  occup = toccup, white = twhite, kings   = tkings,
                  pawns   = tpawns, knights = tknights, queens  = tqueens,
                  rooks   = trooks, bishops = tbishops
               }
    where !toccup = kkrq p .|. diag p
          !tkings = kkrq p .&. diag p `less` slide p
          !twhite = toccup `less` black p
          !tpawns   = diag p `less` (kkrq p .|. slide p)
          !tknights = kkrq p `less` (diag p .|. slide p)
          !tqueens  = slide p .&. kkrq p .&. diag p
          !trooks   = slide p .&. kkrq p `less` diag p
          !tbishops = slide p .&. diag p `less` kkrq p

updatePosAttacs :: MyPos -> MyPos
updatePosAttacs p = p {
        whPAttacs = twhPAtt, whNAttacs = twhNAtt, whBAttacs = twhBAtt,
        whRAttacs = twhRAtt, whQAttacs = twhQAtt, whKAttacs = twhKAtt,
        -- whAttacs = twhPAtt .|. twhNAtt .|. twhBAtt .|. twhRAtt .|. twhQAtt .|. twhKAtt,
        blPAttacs = tblPAtt, blNAttacs = tblNAtt, blBAttacs = tblBAtt,
        blRAttacs = tblRAtt, blQAttacs = tblQAtt, blKAttacs = tblKAtt,
        -- blAttacs = tblPAtt .|. tblNAtt .|. tblBAtt .|. tblRAtt .|. tblQAtt .|. tblKAtt
        whAttacs = twhAttacs, blAttacs = tblAttacs
    }
    where !twhPAtt = foldl' (\w s -> w .|. pAttacs s White)   0 $ bbToSquares $ pawns p .&. white p
          !twhNAtt = foldl' (\w s -> w .|. nAttacs s)     0 $ bbToSquares $ knights p .&. white p
          !twhBAtt = foldl' (\w s -> w .|. bAttacs s (occup p)) 0 $ bbToSquares $ bishops p .&. white p
          !twhRAtt = foldl' (\w s -> w .|. rAttacs s (occup p)) 0 $ bbToSquares $ rooks p .&. white p
          !twhQAtt = foldl' (\w s -> w .|. qAttacs s (occup p)) 0 $ bbToSquares $ queens p .&. white p
          !twhKAtt = kAttacs $ firstOne $ kings p .&. white p
          !tblPAtt = foldl' (\w s -> w .|. pAttacs s Black)   0 $ bbToSquares $ pawns p .&. black p
          !tblNAtt = foldl' (\w s -> w .|. nAttacs s)     0 $ bbToSquares $ knights p .&. black p
          !tblBAtt = foldl' (\w s -> w .|. bAttacs s (occup p)) 0 $ bbToSquares $ bishops p .&. black p
          !tblRAtt = foldl' (\w s -> w .|. rAttacs s (occup p)) 0 $ bbToSquares $ rooks p .&. black p
          !tblQAtt = foldl' (\w s -> w .|. qAttacs s (occup p)) 0 $ bbToSquares $ queens p .&. black p
          !tblKAtt = kAttacs $ firstOne $ kings p .&. black p
          !twhAttacs = twhPAtt .|. twhNAtt .|. twhBAtt .|. twhRAtt .|. twhQAtt .|. twhKAtt
          !tblAttacs = tblPAtt .|. tblNAtt .|. tblBAtt .|. tblRAtt .|. tblQAtt .|. tblKAtt

updatePosCheck :: Bool -> MyPos -> MyPos
updatePosCheck recalc p = p {
                  check = tcheck
                  -- pinned = calcPinned p wpind bpind,
                  -- wpindirs = wpind, bpindirs = bpind
               }
    where -- (wpind, bpind) = if recalc then allPLKAs p else (wpindirs p, bpindirs p)
          !whcheck = white p .&. kings p .&. blAttacs p
          !blcheck = black p .&. kings p .&. whAttacs p
          !tcheck = blcheck .|. whcheck

-- compute the actually pinned pieces based on pining directions and occupancy
-- {-# INLINE calcPinned #-}
calcPinned p wpind bpind = wpi .|. bpi
    where wpi = foldl' (.|.) 0 $ filter ((/= 0) . (.&. white p))
                    $ filter exactOne $ map ((.&. occup p) . snd) bpind
          bpi = foldl' (.|.) 0 $ filter ((/= 0) . (.&. black p))
                    $ filter exactOne $ map ((.&. occup p) . snd) wpind

-- Generate the castle moves
genMoveCast :: MyPos -> Color -> [Move]
genMoveCast p c
    | inCheck p || kingmoved = []
    | otherwise = kingside ++ queenside
    where (ksq, crk, crq, cmidk, cmidq, opAtt) =
             if c == White then (4,  caRKiw, caRQuw, caRMKw, caRMQw, blAttacs p)
                           else (60, caRKib, caRQub, caRMKb, caRMQb, whAttacs p)
          epc = epcas p
          kingmoved = not (epc `testBit` ksq)
          rookk = ksq + 3
          rookq = ksq - 4
          kingside = if (epc `testBit` rookk) && (occup p .&. cmidk == 0) && (opAtt .&. cmidk == 0)
                        then [caks] else []
          queenside = if (epc `testBit` rookq) && (occup p .&. cmidq == 0) && (opAtt .&. cmidq == 0)
                        then [caqs] else []
          caks = makeCastleFor c True
          caqs = makeCastleFor c False

-- Set a piece on a square of the table
setPiece :: Square -> Color -> Piece -> MyPos -> MyPos
setPiece sq c f p = p { basicPos = nbp, zobkey = nzob, mater = nmat }
    where setCond cond = if cond then (`setBit` sq) else (`clearBit` sq)
          bp = basicPos p
          nbp = bp {
                    bpblack = setCond (c == Black) $ black p,
                    bpslide = setCond (isSlide f)  $ slide p,
                    bpkkrq  = setCond (isKkrq f)   $ kkrq p,
                    bpdiag  = setCond (isDiag f)   $ diag p
                }
          oldcon = tabla p sq
          nzob = zobkey p `xor` zold `xor` znew
          nmat = mater p - mold + mnew
          (zold, mold) = case oldcon of
                         Empty      -> (0, 0)
                         Busy co fo -> (zobPiece co fo sq, matPiece co fo)
          znew = zobPiece c f sq
          mnew = matPiece c f

illegalPos :: MyPos -> Bool
illegalPos p = kingsok && checkok
    where nextmoveblack = (epcas p .&. mvMask) /= 0
          nextmovewhite = not nextmoveblack
          whcheck = white p .&. kings p .&. blAttacs p
          blcheck = black p .&. kings p .&. whAttacs p
          kingsok = popCount1 (kings p .&. white p) == 1
                      && popCount1 (kings p .&. black p) == 1
          checkok = nextmoveblack && (whcheck /= 0) || nextmovewhite && (blcheck /= 0)

type ChangeAccum = (ZKey, Int)

-- Accumulate a set of changes in MyPos (except BBoards) due to setting a piece on a square
accumSetPiece :: Square -> Color -> Piece -> MyPos -> ChangeAccum -> ChangeAccum
accumSetPiece sq c f p (z, m) = (z1, m1)
    where oldcon = tabla p sq
          znew = zobPiece c f sq
          mnew = matPiece c f
          (z1, m1) = case oldcon of
               Empty      -> (z `xor` znew, m + mnew)
               Busy co fo -> (z `xor` znew `xor` zobPiece co fo sq,
                                            m + mnew - matPiece co fo)

-- Accumulate a set of changes in MyPos (except BBoards) due to clearing a square
accumClearSq :: Square -> MyPos -> ChangeAccum -> ChangeAccum
accumClearSq sq p i@(z, m) = r
    where oldcon = tabla p sq
          r = case oldcon of
                Empty      -> i
                -- Busy co fo -> (tc : tcs, z `xor` zobPiece co fo sq, m - matPiece co fo sq)
                Busy co fo -> (z `xor` zobPiece co fo sq, m - matPiece co fo)

accumMoving :: MyPos -> ChangeAccum -> ChangeAccum
accumMoving p (z, m) = (z `xor` zobMove, m)

-- Take an initial accumulation and a list of functions accum to accum
-- and compute the final accumulation
chainAccum :: ChangeAccum -> [ChangeAccum -> ChangeAccum] -> ChangeAccum
chainAccum = foldl (flip ($))

changePining :: MyPos -> Square -> Square -> Bool
changePining p src dst = kings p `testBit` src	-- king is moving
                      || slide p `testBit` src -- pining piece is moving
                      || slide p `testBit` dst -- pining piece is captured

clearCast :: Square -> BBoard -> BBoard
clearCast sq bb = if caRiMa `testBit` sq then bb `clearBit` sq else bb

-- Just for a dumb debug: a quick check if two consecutive moves
-- can be part of a move sequence
alternateMoves :: MyPos -> Move -> Move -> Bool
alternateMoves p m1 m2
    | Busy c1 _ <- tabla p src1,
      Busy c2 _ <- tabla p src2 = c1 /= c2
    | otherwise = True	-- means: we cannot say...
    where src1 = fromSquare m1
          src2 = fromSquare m2

legalMove :: MyPos -> Move -> Bool
legalMove p m
    | Busy col fig <- tabla p src = moving p == col && canMove fig p src dst
    | otherwise = False
    where src = fromSquare m
          dst = toSquare m

nonCapt :: MyPos -> Move -> Bool
nonCapt p m
    | Busy _ _ <- tabla p (toSquare m) = False
    | otherwise                        = True

canMove :: Piece -> MyPos -> Square -> Square -> Bool
canMove Pawn p src dst
    | (src - dst) .&. 0x7 == 0 = elem dst $
         map snd $ pAll1Moves col pw (occup p) ++ pAll2Moves col pw (occup p)
    | otherwise = pAttacs src col `testBit` dst
    where col = moving p
          pw = bit src
canMove fig p src dst = fAttacs src fig (occup p) `testBit` dst

-- Copy one square to another and clear the source square
-- doFromToMove :: Square -> Square -> MyPos -> Maybe MyPos
-- {-# INLINE doFromToMove #-}
doFromToMove :: Move -> MyPos -> MyPos
doFromToMove m p | moveIsNormal m = updatePos (changePining p src dst) p {
                                        basicPos = nbp, zobkey = tzobkey, mater = tmater
                                    }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq  = tkkrq,  bpdiag  = tdiag,
              bpepcas = tepcas
          }
          !src = fromSquare m
          !dst = toSquare m
          !shf = dst - src
          !mask = bit dst .|. bit src
          copy w = w `xor` ((w `xor` (shift w shf `clearBit` src)) .&. mask)
          !tblack = copy $ black p
          !tslide = copy $ slide p
          !tkkrq  = copy $ kkrq p
          !tdiag  = copy $ diag p
          !pawnmoving = case tabla p src of
                       Busy _ fig -> fig == Pawn
                       _          -> False	-- actually this is an error!
          !iscapture  = case tabla p dst of
                       Empty -> False
                       _     -> True
          !irevers = pawnmoving || iscapture
          -- Here: we have to xor with the zobrist keys for casts! Only when rights change!
          !tepcas' = clearCast src $ clearCast dst $ epcas p `xor` mvMask	-- to do: ep
          !tepcas  = if irevers then reset50Moves tepcas' else addHalfMove tepcas'
          (tzobkey, tmater) = case tabla p src of	-- identify the moving piece
               Busy col fig -> chainAccum (zobkey p, mater p) [
                                   accumClearSq src p,
                                   accumSetPiece dst col fig p,
                                   accumMoving p
                               ]
               _ -> error $ "Src field empty: " ++ show src ++ " dst " ++ show dst ++ " in pos\n"
                                 ++ showTab (black p) (slide p) (kkrq p) (diag p)
                                 ++ "resulting pos:\n"
                                 ++ showTab tblack tslide tkkrq tdiag
doFromToMove m p | moveIsEnPas m = updatePos False p {
                                       basicPos = nbp, zobkey = tzobkey, mater = tmater
                                   }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq  = tkkrq,  bpdiag  = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          del = moveEnPasDel m
          shf = dst - src
          mask = bit dst .|. bit src
          copy w = w `xor` ((w `xor` (shift w shf `clearBit` src)) .&. mask)
          tblack = copy (black p) `clearBit` del
          tslide = copy (slide p) `clearBit` del
          tkkrq  = copy (kkrq p) `clearBit` del
          tdiag  = copy (diag p) `clearBit` del
          tepcas = reset50Moves $ epcas p `xor` mvMask	-- to do: ep
          Busy col fig = tabla p src	-- identify the moving piece
          Busy co1 fi1 = tabla p del	-- identify the captured piece (pawn)
          (tzobkey, tmater) = chainAccum (zobkey p, mater p) [
                              accumClearSq src p,
                              accumClearSq del p,
                              accumSetPiece dst col fig p,
                              accumMoving p
                          ]
doFromToMove m p | moveIsTransf m = updatePos True p0 {
                                        basicPos = nbp, zobkey = tzobkey, mater = tmater
                                    }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq = tkkrq, bpdiag = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          pie = moveTransfPiece m
          p0 = setPiece src (moving p) pie p
          shf = dst - src
          mask = bit dst .|. bit src
          copy w = w `xor` ((w `xor` (shift w shf `clearBit` src)) .&. mask)
          tblack = copy $ black p0
          tslide = copy $ slide p0
          tkkrq  = copy $ kkrq p0
          tdiag  = copy $ diag p0
          tepcas = reset50Moves $ epcas p `xor` mvMask	-- to do: ep
          Busy col fig = tabla p0 src	-- identify the moving piece
          (tzobkey, tmater) = chainAccum (zobkey p0, mater p0) [
                              accumClearSq src p0,
                              accumSetPiece dst col pie p0,	--- Hier: is this ok???
                              accumMoving p
                          ]
doFromToMove m p | moveIsCastle m = updatePos True p {
                                        basicPos = nbp, zobkey = tzobkey, mater = tmater
                                    }
    where nbp = BPos {
              bpblack = tblack, bpslide = tslide, bpkkrq  = tkkrq,  bpdiag  = tdiag,
              bpepcas = tepcas
          }
          src = fromSquare m
          dst = toSquare m
          (csr, cds) = moveCastleFromTo m
          shf = dst - src
          shfr = cds - csr
          mask = bit dst .|. bit src
          maskr = bit cds .|. bit csr
          copy w = w `xor` ((w `xor` (shift w shf `clearBit` src)) .&. mask)
          copyr w = w `xor` ((w `xor` (shift w shfr `clearBit` csr)) .&. maskr)
          tblack = copyr $ copy $ black p
          tslide = copyr $ copy $ slide p
          tkkrq  = copyr $ copy $ kkrq p
          tdiag  = copyr $ copy $ diag p
          -- Here: we have to xor with the zobrist keys for casts! Only when rights change!
          tepcas = reset50Moves $ clearCast src $ epcas p `xor` mvMask	-- to do: ep
          Busy col fig = tabla p src	-- identify the moving piece (king)
          Busy co1 fi1 = tabla p csr	-- identify the moving rook
          (tzobkey, tmater) = chainAccum (zobkey p, mater p) [
                              accumClearSq src p,
                              accumSetPiece dst col fig p,
                              accumClearSq csr p,
                              accumSetPiece cds co1 fi1 p,
                              accumMoving p
                          ]

reverseMoving :: MyPos -> MyPos
reverseMoving p = p { basicPos = nbp, zobkey = z }
    where nbp = (basicPos p) { bpepcas = tepcas }
          tepcas = epcas p `xor` mvMask
          (z, _) = chainAccum (zobkey p, mater p) [
                       accumMoving p
                   ]
-- Here is not clear what to do with castle and en passant...
