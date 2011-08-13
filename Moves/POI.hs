module POI where

-- POI is a point of interest, i.e. a data structure around an important square
data POI = POI {
               poiCenter    :: !Square,	-- central square of the poi
               poiWAttacks  :: !BBoard,	-- bitboard of center attacking white pieces, inclusiv x-ray
               poiBAttacks  :: !BBoard,	-- bitboard of center attacking black pieces, inclusiv x-ray
               poiBlockers  :: !BBoard,	-- bitboard of blocking squares (which, when freed, could influence the poi
               poiKInf      :: !BBoard,	-- bitboard of all squares on which a king influences the poi
               poiRInf      :: !BBoard,	-- bitboard of all squares on which a queen or a rook influences the poi
               poiBInf      :: !BBoard,	-- bitboard of all squares on which a queen or a bishop influences the poi
               poiNInf      :: !BBoard,	-- bitboard of all squares on which a knight influences the poi
               poiWPInf     :: !BBoard,	-- bitboard of all squares on which a black(!) pawn influences the poi
               poiBPInf     :: !BBoard,	-- bitboard of all squares on which a white(!) pawn influences the poi
               poiSEEPcs    :: !BBoard,	-- bitboard of squares which can move SEE non-negative to the center
               poiSEEMvs    :: [Move]	-- move list of SEE non-negative moves to the center
               }

-- We must have two arrays of bitboards, which should be precalculated (every taking 2^6 x 2^6 bbs = 4K bbs = 32 KB)
-- one of the form 'bbSquaresBetween!(sq1,sq2)' which give the bitboard of all squares between the two square indices
-- one of the form 'bbFromToFurther!(sq1,sq2)' which gives the bitboard of all squares from sq1 to sq2 and further
-- (actually the first could be calculated from the second as intersection sq1->sq2 and sq2->sq1, but for performance...)

-- Recalculate a poi given a position and the old poi
-- Center and influence squares for non-sliding pieces never change (so they are copied from the old poi)
-- Blockers and influence squares for slider have always to be recomputed
-- The rest, only when the attackers change (so attackers have also to be recalculated, at least for that check)
poiRecalc :: MyPos -> POI -> POI
poiRecalc p opoi = if poiWAttacks opoi == poiaw && poiBAttacs opoi == poiab
                      then opoi { poiBlockers = poib, poiRInf = atrs, poiBInf = atbs }
                      else npoi
    where sq = poiCenter opoi
          npoi = opoi {
                        poiWAttacks = poiaw, poiBAttacks = poiab, poiBlockers = poib,
                        poiRInf = atrs, poiBInf = atbs, poiSEEPcs = poisee, poiSEEMvs = poim
                     }
          ocpb = occup p `less` bishops p `less` queens p
          ocpr = occup p `less` rooks p `less` queens p
          atrs = rAttacs sq ocpr	-- rook attacs from sq
          atbs = bAttacs sq ocpb	-- bishop attacs from sq
          atqs = atrs .|. atbs		-- queen attacs from sq
          atsl = atqs .&. queens p .|. atrs .&. rooks p .|. atbs .&. bishops p	-- all attacking sliders
          poia =     poiKInf opoi .&. kings p
                 .|. poiNInf opoi .&. knights p
                 .|. poiBPInf opoi .&. pawns p .&. white p	-- black & white trick because we look
                 .|. poiWPInf opoi .&. pawns p .&. black p	-- from the sq point of view
                 .|. atsl
          poiaw = poia .&. white p
          poiab = poia .&. black p
          poib = atqs .&. occup p
          (poisee, poim) = calcPoiSee p sq poia

-- When moving a piece, there is always a chance that this piece will become itself
-- the center of a new poi, and for the king and adiacent squares, this is always the case
-- This function returns a new poi when either it's a permanent one or only when there are attackers
poiCreate :: MyPos -> Square -> Bool -> Maybe POI
poiCreate p sq force
    | force || poia /= 0 = Just newPoi
    | otherwise          = Nothing
    where newPoi = POI {
                          poiCenter = sq, poiWAttacks = poiaw, poiBAttacks = poiab, poiBlockers = poib,
                          poiKInf = poiki, poiRInf = atrs, poiBInf = atbs, poiNInf = poini,
                          poiWPInf = poipw, poiBPInf = poipb, poiSEEPcs = poisee, poiSEEMvs = poim
                       }
          ocpb = occup p `less` bishops p `less` queens p
          ocpr = occup p `less` rooks p `less` queens p
          atrs = rAttacs sq ocpr	-- rook attacs from sq
          atbs = bAttacs sq ocpb	-- bishop attacs from sq
          atqs = atrs .|. atbs		-- queen attacs from sq
          atsl = atqs .&. queens p .|. atrs .&. rooks p .|. atbs .&. bishops p	-- all attacking sliders
          poiki = kAttacs sq
          poini = nAttacs sq
          poipw = pAttacs sq White
          poipb = pAttacs sq Black
          poia =     poiki .&. kings p
                 .|. poini .&. knights p
                 .|. poipb .&. pawns p .&. white p	-- black & white trick because we look
                 .|. poipw .&. pawns p .&. black p	-- from the sq point of view
                 .|. atsl
          poib = atqs .&. occup p
          (poisee, poim) = calcPoiSee p sq poia

-- Given a piece, a square, an action (come or go) and a poi: when does the action influence
-- the poi, so that it is necessary to recalculate it?
-- Case 1: square is the central square of the poi
-- Case 2: square is one of the attackers of the poi
-- Case 3: piece comes in the influence zone corresponding to the piece type
-- Case 4: one blocker leaves (square is in blockers and action is leave)
-- If we check exactly in this order then we don't need to be very exact about blockers and influence zones

data PieceAction = Come | Leave

poiIsChanging :: MyPos -> POI -> Piece -> PieceAction -> Square -> Bool
poiIsChanging pos poi piece pa sq
    =    sq == poiCenter poi						-- case 1
      || poiWAttacks poi `testBit` sq || poiBAttacks poi `testBit` sq	-- case 2
      || pa == Come && (ro || bi || ni || pi || ki)			-- case 3
      || pa == Leave && poiBlockers poi `testBit` sq && discoverSlider pos (poiCenter poi) sq	-- case 4
    where ro = poiRInf poi `testBit` sq && (piece == Rook || piece == Queen)
          bi = poiBInf poi `testBit` sq && (piece == Bishop && piece == Queen)
          ni = poiNInf poi `testBit` sq && piece == Knight
          pi = piece == Pawn && (poiWPInf poi `testBit` sq || poiBPInf poi `testBit` sq)
          ki = poiKInf poi `testBit` sq && piece == King

-- When moving a blocker, it can be that a slider behind that blocker comes free and attacs the center
-- This function checks if this can be the case
-- We check at least if there is a slider in the correct place, but it could be either obscured or the
-- wrong type (e.g. rook on the diagonal). We do not check exactly, because this is as hard as recalculating
-- the poi, which we prefer (at the risk that the recalculation was not necessary).
-- We could at least check the slider type, but when we would need to code also the direction between center
-- and blocker - this could be the next improvement
discoverSlider :: MyPos -> Square -> Square
discoverSlider pos csq sq = further /= 0 && further .&. sliders pos /= 0
    where further = bbFromToFurther!(csq, sq) `less` bbSquaresBetween!(csq, sq)

-- Now the philosofy of keeping pois up to date:
-- The list of pois is part of the position (should be a map or an array?)
-- When making a move, the following has to take place:
-- 1. mark for deletion all the pois which are "moved", i.e. the from-square is the center
--    (normally one poi, but for en passant 2 pois are involved, and for castle - 10 - one for the rook
--    and 9 for the king! By the way, a good reason to sort king moves at the end, in the hope they will not be searched.
--    Another idea: the 8 neighbours of the king should be calculated only when there is enough material for king safety)
-- 2. create a list of all triplets piece - piece action - square involved in the move and run poiIsChanging for everyone,
--    collecting a list of pois to be recalculated
-- 3. for non captures: create (conditionally) a poi for the new locations involved in the move (same remark as pct.1)
-- 4. Modify the list of pois accordingly: delete, add (poiCreate), recalculate (poiRecalc)

-- So why are the pois good?
-- 1. First, for move generation: we can generate the winning and losing moves once per poi, and then just copy
--    them as needed - as log as the poi is stable, we don't need recalculation
-- 2. For evaluation of:
--    2a. king safety - counting the winning attacs on the king neighbours and the non winning attacs (which should be
--        rated lower) we gain a more clear view of the king safety
--    2b. position stability - counting equal or just slighty loosing exchanges can give us an idea how stable our
--        position is (if it's stable, the positional factors are more trustfully)
-- 3. Finding weak points: this could guide the move ordering