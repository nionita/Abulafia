{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import Control.Monad
import Data.Char (isSpace)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO
import System.Time

import Struct.Struct
import Struct.Status
import Moves.Moves
import Moves.Board
import Moves.BaseTypes
import Moves.Base
import Moves.History
import Eval.Eval
import Hash.TransTab
import Search.AlbetaTypes
import Search.Albeta
-- import Search.SearchAB
import Search.SearchMonad
import Config.ConfigClass
import Config.Config
-- import Moves.Notation

-- Here we work in the old plain IO monad:
instance CtxMon IO where
    tellCtx = tellInIO
    timeCtx = return 0

-- This is a fen list from the test suite STS (selection)
-- which we will use as default test suite
epdList = [
    ("1kr5/3n4/q3p2p/p2n2p1/PppB1P2/5BP1/1P2Q2P/3R2K1 w - - 0 1", "f5", "Undermine.001", "f5=10, Be5+=2, Bf2=3, Bg4=2"),
    ("1n5k/3q3p/pp1p2pB/5r2/1PP1Qp2/P6P/6P1/2R3K1 w - - 0 1", "c5", "Undermine.002", "c5=10, Qd4+=4, b5=4, g4=3"),
    ("1n6/4bk1r/1p2rp2/pP2pN1p/K1P1N2P/8/P5R1/3R4 w - - 0 1", "c5", "Undermine.003", "c5=10, Rd3=7, Rdd2=7, Rg3=7 Rd5=9"),
    ("1nr5/1k5r/p3pqp1/3p4/1P1P1PP1/R4N2/3Q1PK1/R7 w - - 0 1", "b5", "Undermine.004", "b5=10, Kg3=4, Ng5=4, Qe3=4"),
    ("1q2r1k1/1b2bpp1/p2ppn1p/2p5/P3PP1B/2PB1RP1/2P1Q2P/2KR4 b - - 0 1", "c4", "Undermine.005", "c4=10, Bc6=7, Qa8=7, Qc8=7"),
    ("1q4k1/5p1p/p1rprnp1/3R4/N1P1P3/1P6/P5PP/3Q1R1K w - - 0 1", "e5", "Undermine.006", "e5=10, Nc3=3, Qd3=1, Qf3=2"),
    ("1qr1k2r/1p2bp2/pBn1p3/P2pPbpp/5P2/2P1QBPP/1P1N3R/R4K2 b k - 0 1", "h4", "Undermine.007", "h4=10, Bd8=1, Bf8=1, Rh7=1"),
    ("1b1r4/3rkp2/p3p2p/4q3/P5P1/2RBP3/P1Q4P/1R3K2 b - - 0 1", "Ba7", "Ba7=10, Qf6+=3, a5=3, h5=5", "STS(v2.2) Open Files and Diagonals.001"),
    ("1bq3rk/R6p/5n1P/1N1p4/1PnP2p1/6P1/5B2/2Q2BK1 w - - 0 1", "Re7", "Re7=10, Ra2=5, Rg7=2", "STS(v2.2) Open Files and Diagonals.002"),
    ("1k1r3r/1p1b1Q1p/p7/q3p3/4p3/2P1N3/P4PPP/R4RK1 w - - 0 1", "Rad1", "Rad1=10, Qf6=7, Rfd1=7, a4=2", "STS(v2.2) Open Files and Diagonals.003"),
    ("1Q6/1b4pk/2q2b1p/1p1ppP2/1Pp5/2P2P1P/2BB2P1/6K1 b - - 0 1", "Qa6", "Qa6=10, Bc8=5, Qd7=7", "STS(v2.2) Open Files and Diagonals.004"),
    ("1qrr3k/1p2bp1p/1n2p1pP/p2pP3/P4B2/1PPB2P1/2R1QP2/3R2K1 w - - 0 1", "Bb5", "Bb5=10, Qe1=2, Qe3=2", "STS(v2.2) Open Files and Diagonals.005"),
    ("1r1n1rk1/3qp2p/P2p2p1/1p6/5pP1/1p3P1P/5PB1/R1QR2K1 w - - 0 1", "Bf1", "Bf1=10, Qb2=7, Qc3=7, Qd2=6", "STS(v2.2) Open Files and Diagonals.006"),
    ("1r1n2k1/5r1p/P2qp1p1/3p4/1p3pP1/1Q3P1P/R4P2/2R2BK1 w - - 0 1", "Rac2", "Rac2=10, Kg2=5, Qa4=4", "STS(v2.2) Open Files and Diagonals.007"),
    ("1k2r2r/1bq2p2/pn4p1/3pP3/pbpN1P1p/4QN1B/1P4PP/2RR3K b - - 0 1", "Nd7", "Nd7=10, Bc5=8, Bc6=2, Be7=7", "STS: Knight Outposts/Repositioning/Centralization.001"),
    ("1q2bn2/6pk/2p1pr1p/2Q2p1P/1PP5/5N2/5PP1/4RBK1 w - - 0 1", "Ne5", "Ne5=10, Nd4=8, Ra1=6, b5=9", "STS: Knight Outposts/Repositioning/Centralization.002"),
    ("1r1q1rk1/1b1n1p1p/p2b1np1/3pN3/3P1P2/P1N5/3BB1PP/1R1Q1RK1 b - - 0 1", "Ne4", "Ne4=10, Bxa3=6, Nb6=6", "STS: Knight Outposts/Repositioning/Centralization.003"),
    ("1r1r1bk1/1bq2p1p/pn2p1p1/2p1P3/5P2/P1NBB3/1P3QPP/R2R2K1 b - - 0 1", "Nd5", "Nd5=10, Ba8=8, Kg7=8, a5=9", "STS: Knight Outposts/Repositioning/Centralization.004"),
    ("1r1r2k1/5pp1/p2p4/1p2pnqp/1BP1Q3/PP1R2P1/5P1P/3R2K1 b - - 0 1", "Nd4", "Nd4=10, Qf6=5, bxc4=3, h4=3", "STS: Knight Outposts/Repositioning/Centralization.005"),
    ("1r1r4/R3pk2/4n1p1/2p2p2/8/4B3/Pn2BPPP/5RK1 b - - 0 1", "Nd4", "Nd4=10, Nd3=1, c4=4, f4=9", "STS: Knight Outposts/Repositioning/Centralization.006"),
    ("1r2k2r/pp2ppb1/2n2np1/7p/4P3/P3BB1P/1P1N1PP1/R2R2K1 b k - 0 1", "Nd7", "Nd7=10, Bh6=6, a6=6", "STS: Knight Outposts/Repositioning/Centralization.007"),
    ("1r2qrk1/3bn3/pp1p3p/n1p1p1p1/P1P5/B1PP1NPP/2Q2PB1/1R2R1K1 w - - 0 1", "Nd2", "Nd2=10, Bc1=6, Qe2=9, Rb2=2", "STS: Knight Outposts/Repositioning/Centralization.008"),
    ("6k1/p2pp2p/bp4n1/q1r4R/1RP1P3/2P2B2/P2Q2P1/4K3 w - - 0 1", "Rd5", "Rd5=10, Rf5=6, g4=7", "STS(v4.0) Square Vacancy.001"),
    ("r2r2k1/pp3ppp/2p1qn2/5N1b/1n2PP2/4Q2P/PPP3B1/R1B2RK1 w - - 0 1", "Qc5", "Qc5=10, Qg3=3", "STS(v4.0) Square Vacancy.002"),
    ("3r4/p4pk1/P1pr3p/3nb3/1p6/5B1P/1P3PP1/R1BR2K1 w - - 0 1", "Ra5", "Ra5=10, Kf1=2, g3=5", "STS(v4.0) Square Vacancy.003"),
    ("1b1r3r/3pkpp1/3np1q1/2p5/2P1PPP1/5Q2/PP4B1/R1BR2K1 b - - 0 1", "Rh4", "Rh4=10, Nxc4=6, Rc8=7", "STS(v4.0) Square Vacancy.004"),
    ("7k/1p6/1n1rrq1p/1R1p1p1p/3P1P2/QR2P2P/6PK/5B2 w - - 0 1", "Qa7", "Qa7=10, Kg1=7, R5b4=7", "STS(v4.0) Square Vacancy.005"),
    ("5r1k/5rp1/p1n1p1qp/2P1p3/P7/4QN1P/5PP1/2R1R2K w - - 0 1", "Rc4", "Rc4=10, Qe2=5, Rc3=7", "STS(v4.0) Square Vacancy.006"),
    ("6r1/1p2Q1pk/2p2p1p/3p1P1P/p1nP4/PqP1P3/1P2RN2/2K5 b - - 0 1", "Qa2", "Qa2=10, Qb5=5, b6=3", "STS(v4.0) Square Vacancy.007"),
    ("1b3rk1/5ppp/2p2rq1/1p1n4/3P2P1/1BPbBP2/1P1N2QP/R3R1K1 w - - 0 1", "Bxd5", "Bxd5=10, Ne4=6", "STS(v5.0) Bishop vs Knight.001"),
    ("1k1r2r1/1p2bp2/4q2p/p1ppP3/6b1/2PQ2B1/PP2NPP1/R1R3K1 b - - 0 1", "Bxe2", "Bxe2=10, Bf5=3, Bg5=3, c4=4", "STS(v5.0) Bishop vs Knight.002"),
    ("1q2n1k1/r4pb1/6p1/1bpPN1Pp/p1N1PP2/3B2KP/R3Q3/8 b - - 0 1", "Bxe5", "Bxe5=10", "STS(v5.0) Bishop vs Knight.003"),
    ("1r1qr1k1/1b1nbpp1/ppnp3p/8/2P1PBB1/2N5/PPNQ2PP/2R2RK1 w - - 0 1", "Bxd7", "Bxd7=10, Be2=2, Ne3=3, Rcd1=3", "STS(v5.0) Bishop vs Knight.004"),
    ("1r1r2k1/2qp1ppp/2pbpn2/8/3BP3/3B2P1/P1P2P1P/R2Q1RK1 w - - 0 1", "Bxf6", "Bxf6=10, Qd2=7, Qe1=6, a4=5", "STS(v5.0) Bishop vs Knight.005"),
    ("1r1r2n1/1pb3pk/p1p3bp/4Np2/3P2P1/2N1RB1P/PP3PK1/4R3 w - - 0 1", "Nxg6", "Nxg6=10, Ne2=3, Rd1=2, d5=3", "STS(v5.0) Bishop vs Knight.006"),
    ("1r2r1k1/2q3p1/3bp2p/p2nNp2/1ppP1P2/2P3Q1/PP1RN1PP/R5K1 b - - 0 1", "Bxe5", "Bxe5=10, Rb5=7, Rb7=6, a4=6", "STS(v5.0) Bishop vs Knight.007"),
    ("1r2rbk1/1bqn1pp1/pp1p2np/3N1p2/2P1P3/PP2B1PB/4NQKP/2RR4 b - - 0 1", "Bxd5", "Bxd5=10, Qc6=6, Qc8=6, Qd8=3", "STS(v5.0) Bishop vs Knight.008"),
    ("1k1r1r2/p1p5/Bpnbb3/3p2pp/3P4/P1N1NPP1/1PP4P/2KR1R2 w - - 0 1", "Ncxd5", "STS(v6.0) Recapturing.001", "Ncxd5=10, Nb5=4, Ne2=3, Nexd5=6"),
    ("1k1r4/4rp2/1p1qbnp1/p2p3p/P2P4/1Nn2P2/1P1QBRPP/2R3K1 w - - 0 1", "Rxc3", "STS(v6.0) Recapturing.002", "Rxc3=10, Qxc3=5, Re1=8, bxc3=7"),
    ("1k1rr3/pp2qpp1/1b2p3/4N2p/2R1n2P/P2R2P1/1PP1QP2/1K6 w - - 0 1", "Rxe4", "STS(v6.0) Recapturing.003", "Rxe4=10, Ng6=7, Rxd8+=3"),
    ("1k3rn1/3r1p2/p2p1np1/Pp1P3p/1Pp1PP2/2P1R1bP/2B2K1B/6R1 w - - 0 1", "Bxg3", "STS(v6.0) Recapturing.004", "Bxg3=10, Kxg3=5, Rexg3=5, Rgxg3=3"),
    ("1q2rnk1/5rb1/bp1p1np1/pNpP2Bp/P1P1Pp1P/3B2P1/3QNRR1/7K w - - 0 1", "gxf4", "STS(v6.0) Recapturing.005", "gxf4=10, Bxf4=8, Bxf6=7, Rxf4=7"),
    ("1q4k1/1r3pp1/3p1n1p/2pPpP2/b1PnP3/rPB1R1NP/3Q2PK/1R3B2 b - - 0 1", "Bxb3", "STS(v6.0) Recapturing.006", "Bxb3=10, Raxb3=2, Rbxb3=3"),
    ("1qr2rk1/4ppbp/6p1/pp1b4/2NP4/4B3/PPB2PPP/2RQR2K b - - 0 1", "bxc4", "STS(v6.0) Recapturing.007", "bxc4=10, Bxc4=3, Rfd8=6, Rxc4=5"),
    ("1qrr3k/6p1/1p1pp2p/pNn5/Pn1bP1PP/5Q2/1PP1N3/1K1R2R1 w - - 0 1", "Nexd4", "STS(v6.0) Recapturing.008", "Nexd4=10, Nbxd4=4, Rg2=5"),
    ("1R3b2/r4pk1/2qpn1p1/P1p1p2p/2P1P2P/5PP1/6K1/1Q1BB3 w - - 0 1", "Qb6", "STS(v7.0) Simplification.001", "Qb6=10, Bc3=7, Kf1=7, Qb5=9"),
    ("1b4k1/6p1/pr2p2p/5p2/P2N4/P1Q1P1qP/6P1/5RK1 b - - 0 1", "Be5", "STS(v7.0) Simplification.002", "Be5=10, Bd6=3, Kh7=4, Qh2+=7"),
    ("1k1r1b1r/ppqbnp2/4p1pp/n2pP3/2pP4/P1P3B1/1P1NBPPP/1R1QRNK1 b - - 0 1", "Nf5", "STS(v7.0) Simplification.003", "Nf5=10"),
    ("1nb2rk1/2p1pqb1/1r2p2p/R6P/1PBP2p1/B4NN1/5PK1/3QR3 w - - 0 1", "Rf5", "STS(v7.0) Simplification.004", "Rf5=10"),
    ("1nr3k1/1pr2pp1/p3bn2/P3p2p/1PP4P/3BNPP1/3RN1K1/R7 w - - 0 1", "Bf5", "STS(v7.0) Simplification.005", "Bf5=10, Nc3=1, Rdd1=2, b5=1"),
    ("1q1rb1k1/5p1p/p3pnpb/P1r5/2B1P3/1P3P2/2R1N1PP/R2NQ2K b - - 0 1", "Bb5", "STS(v7.0) Simplification.006", "Bb5=10"),
    ("1r1r2k1/2p1qpp1/pnP1b2p/1p2P3/3N4/P3P2P/1QB2PP1/3RR1K1 w - - 0 1", "Qb4", "STS(v7.0) Simplification.007", "Qb4=10, Bd3=7, Qc3=8, f4=9"),
    ("1qr2k1r/pb3pp1/1b2p2p/3nP3/1p6/3B2QN/PP3PPP/R1BR2K1 b - - 0 1", "g5", "STS(v8.0) AKPC.001", "g5=10, Bd4=4, Kg8=4, Rd8=3"),
    ("1r1qr1k1/p5b1/2p2ppp/3p4/1Pp5/P1Nn1Q2/3BN1PP/R4RK1 b - - 0 1", "f5", "STS(v8.0) AKPC.002", "f5=10, a5=6, d4=7, Rb7=5"),
    ("1r1rq1k1/1p4p1/pNb1pp1p/1pP5/3P4/1PQ5/5PPP/R3R1K1 w - - 0 1", "f3", "STS(v8.0) AKPC.003", "f3=10, Qd3=4, Rad1=4, Red1=4"),
    ("1r3r2/1p3q1k/1Q1p4/2pNbpp1/2P5/7P/PP2R1P1/3R3K b - - 0 1", "g4", "STS(v8.0) AKPC.004", "g4=10, f4=7, Ra8=3, Rbe8=6"),
    ("1r4k1/2p2p1p/4n1p1/2qpP3/2nN4/1BPQ4/Pr3PPP/3RR1K1 w - - 0 1", "h4", "STS(v8.0) AKPC.005", "h4=10, g3=7, h3=8, Qh3=9"),
    ("1r4k1/3q2pp/2np4/2p1pp2/2P1P3/R1BP1Q1P/5PP1/6K1 b - - 0 1", "f4", "STS(v8.0) AKPC.006", "f4=10, fxe4=8, Ne7=6, Rb1+=5"),
    ("1r4k1/ppq2ppp/r4nn1/P2p4/2pP4/B1P1P1PP/1Q1N1PK1/RR6 b - - 0 1", "h5", "STS(v8.0) AKPC.007", "h5=10, h6=1, Ne7=1, Nh5=1"),
    ("1rb1r1k1/4qpb1/p1np1npp/1pp5/2P1PN2/1P3PP1/PB4BP/1QRR1N1K b - - 0 1", "g5", "STS(v8.0) AKPC.008", "g5=10, Bb7=5, bxc4=5, Ne5=6"),
    ("1b2r1k1/1bqn1pp1/p1p4p/Pp2p3/1P2B3/2B1PN1P/5PP1/1Q1R2K1 b - - 0 1", "c5", "STS(v9.0) Advancement of a/b/c pawns.001", "c5=10, Kf8=1, Nf8=2, g5=2"),
    ("1k4nr/pppr1q2/3p2p1/3Nn1Qp/2P1PN2/1P6/P1P3PR/2KR4 w - - 0 1", "c5", "STS(v9.0) Advancement of a/b/c pawns.002", "c5=10, Nd3=5, Rf1=2, Rhh1=3"),
    ("1n4k1/2pb2q1/1p1p3p/3P1p2/1PP1pP2/3rN3/4N1PP/1RQ3K1 w - - 0 1", "b5", "STS(v9.0) Advancement of a/b/c pawns.003", "b5=10"),
    ("1nr5/p2p1qpk/1pb1p2p/5p1P/1PP2N2/P3PPQ1/6P1/3RKB2 w - - 0 1", "b5", "STS(v9.0) Advancement of a/b/c pawns.004", "b5=10, Kf2=5, Ng6=6, a4=5"),
    ("1q2r1k1/1b2bpp1/p2ppn1p/2p5/P3PP1B/2PB1RP1/2P1Q2P/2KR4 b - - 0 1", "c4", "STS(v9.0) Advancement of a/b/c pawns.005", "c4=10, Bc6=5, Qa7=4, Qa8=5, Qc8=5, b8a8=5"),
    ("1q2rb2/3b1r1k/p1p4p/B3p1p1/1PPpN3/3P1P1P/3QR1P1/4R1K1 w - - 0 1", "c5", "STS(v9.0) Advancement of a/b/c pawns.006", "c5=10, Kh1=7, Qb2=6, Ra1=7"),
    ("1r1qr1k1/1p3pp1/p1n1bn1p/2NN4/4P3/6P1/PP3PB1/2RQR1K1 w - - 0 1", "b3", "STS(v9.0) Advancement of a/b/c pawns.007", "b3=10, Qd2=5, Rf1=7, a4=5"),
    ("1r1r1bk1/1bq2ppp/pnp1p3/4P3/5P2/P1NBB3/1P4PP/R1QR2K1 b - - 0 1", "c5", "STS(v9.0) Advancement of a/b/c pawns.008", "c5=10, Nd5=5, Rbc8=5, h6=2"),
    ("1b1qrr2/1p4pk/1np4p/p3Np1B/Pn1P4/R1N3B1/1Pb2PPP/2Q1R1K1 b - - 0 1", "Bxe5", "STS(v10.0) Simplification.001", "Bxe5=10, f4=3, Nc4=2"),
    ("1k1r2r1/1b4p1/p4n1p/1pq1pPn1/2p1P3/P1N2N2/1PB1Q1PP/3R1R1K b - - 0 1", "Nxf3", "STS(v10.0) Simplification.002", "Nxf3=10, Rge8=7, Rgf8=6, Rh8=7"),
    ("1k1r3r/pb1q2p1/B4p2/2p4p/Pp1bPPn1/7P/1P2Q1P1/R1BN1R1K b - - 0 1", "Bxa6", "STS(v10.0) Simplification.003", "Bxa6=10, Qc6=3, Qe6=3, Rde8=5"),
    ("1k1r4/1br2p2/3p1p2/pp2pPb1/2q1P2p/P1PQNB1P/1P4P1/1K1RR3 b - - 0 1", "Qxd3+", "STS(v10.0) Simplification.004", "Qxd3+=10, Qa4=6, Qb3=5, Qc5=6"),
    ("1k1r4/1br2p2/3p1p2/pp2pPb1/4P2p/P1PRNB1P/1P4P1/1K2R3 b - - 0 1", "Bxe3", "STS(v10.0) Simplification.005", "Bxe3=10, b4=7, Ka7=6, Rc5=6"),
    ("1k1r4/5qp1/p1b2n1p/1p2p3/2p1P2P/P1N1P1P1/1PB1Q3/3R2K1 b - - 0 1", "Rxd1+", "STS(v10.0) Simplification.006", "Rxd1+=10, Qc7=4, Qe7=4, Qf8=4"),
    ("1k1rr3/p1p4p/Bpnb4/3p1qp1/N2P4/P2Q1PPb/1PP3NP/2KR2R1 w - - 0 1", "Qxf5", "STS(v10.0) Simplification.007", "Qxf5=10, Bb5=3, Nc3=5, Qd2=1"),
    ("1k2r3/1p1bP3/2p2p1Q/Ppb5/4Rp1P/2q2N1P/5PB1/6K1 b - - 0 1", "Kc7", "STS(v11.0) King Activity.001", "Kc7=10, Kc8=4, Qa1+=4"),
    ("1k2r3/p7/Pp1pP1p1/4p2p/2r4P/5P1B/4RB1K/8 w - - 0 1", "Kg3", "STS(v11.0) King Activity.002", "Kg3=10, Kg2=5, Re1=5, Re3=3"),
    ("1k5r/6p1/p2b4/7p/2r1p2P/R1B1P3/6P1/2R3K1 b - - 0 1", "Ka7", "STS(v11.0) King Activity.003", "Ka7=10, Bxa3=3, Kb7=4, Rc6=2"),
    ("1n5k/3r2p1/2p1qp1p/3p1N1P/1P1P1rP1/p1R5/P7/1KRQ4 w - - 0 1", "Ka1", "STS(v11.0) King Activity.004", "Ka1=10, Re3=2, Rg3=3"),
    ("1r2r3/1p1b3k/2p2n2/p1Pp4/P2N1PpP/1R2p3/1P2P1BP/3R2K1 b - - 0 1", "Kg6", "STS(v11.0) King Activity.005", "Kg6=10, Kh6=8, Ne4=8, Nh5=8"),
    ("1r2rqk1/8/bn1p3p/B1p2p2/p1PnpP2/P3R2Q/1P3NPP/2R2BK1 b - - 0 1", "Kh7", "STS(v11.0) King Activity.006", "Kh7=10, Re6=3"),
    ("1R3bk1/7p/6p1/8/1pN3P1/8/r4P1P/6K1 b - - 0 1", "Kf7", "STS(v11.0) King Activity.007", "Kf7=10, Kg7=1"),
    ("1k1r4/4bp2/p1q1pnr1/6B1/NppP3P/6P1/1P3P2/2RQR1K1 w - - 0 1", "Re5", "STS(v12.0) Center Control.001", ""),
    ("1k5r/1pq1b2r/p2p1p2/4n1p1/R3P1p1/1BP3B1/PP1Q3P/1K1R4 w - - 0 1", "Bd5", "STS(v12.0) Center Control.002", ""),
    ("1kb4r/1p3pr1/3b1p1p/q2B1p2/p7/P1P3P1/1P1Q2NP/K2RR3 b - - 0 1", "Be5", "STS(v12.0) Center Control.003", ""),
    ("1kr5/1b3ppp/p4n2/3p4/2qN1P2/2r2B2/PQ4PP/R2R3K b - - 0 1", "Ne4", "STS(v12.0) Center Control.004", ""),
    ("1n1r4/p1q2pk1/b2bp2p/4N1p1/3P1P2/1QN1P3/5PBP/1R5K w - - 0 1", "Ne4", "STS(v12.0) Center Control.005", ""),
    ("1n1rr1k1/1pq2pp1/3b2p1/2p3N1/P1P5/P3B2P/2Q2PP1/R2R2K1 w - - 0 1", "Ne4", "STS(v12.0) Center Control.006", ""),
    ("1n1rr1k1/5pp1/1qp4p/3p3P/3P4/pP1Q1N2/P1R2PP1/1KR5 w - - 0 1", "Ne5", "STS(v12.0) Center Control.007", ""),
    ("1k1r4/1p3p2/p1bq1p1p/4p3/3r1P1Q/P5P1/1PP1B2P/K2RR3 b - - 0 1", "e4", "STS(v13.0) Pawn Play in the Center.001", "e4=10, Qc5=7, Qe6=7, Qe7=7, Re8=7"),
    ("1k2r1r1/ppp1q1b1/nn1pp3/1N3p1p/1PP2P2/PQ2B1PB/4PK1P/3R2R1 b - - 0 1", "e5", "STS(v13.0) Pawn Play in the Center.002", "e5=10, Rgf8=3"),
    ("1kr3r1/1p1nqp2/p2p4/3PpBp1/1PP5/5R1P/P3QP2/R5K1 b - - 0 1", "e4", "STS(v13.0) Pawn Play in the Center.003", "e4=10"),
    ("1q1r2k1/3n1bp1/1p2pp2/pNn5/P3PP2/1P2QB2/6NP/3R2K1 w - - 0 1", "e5", "STS(v13.0) Pawn Play in the Center.004", "e5=10, Be2=5, Nd4=6, Nd6=3, Rb1=7"),
    ("1q2rrk1/p5bp/2p1p1p1/3p4/5P2/4QBP1/PPP2R1P/1R4K1 b - - 0 1", "e5", "STS(v13.0) Pawn Play in the Center.005", "e5=10, a6=1, Qc7=2, Rf7=1"),
    ("1qr1r1k1/5pp1/1p2p2p/1Qbn3b/2R5/3P1NPP/3NPPB1/1R4K1 w - - 0 1", "e4", "STS(v13.0) Pawn Play in the Center.006", "e4=10, d4=2, Ne4=2, Rbc1=1"),
    ("1qr1r3/5ppk/1p2p2p/1Qbn3b/2R5/3P1NPP/3NPPB1/1R5K w - - 0 1", "d4", "STS(v13.0) Pawn Play in the Center.007", "d4=10, e3=3, Kg1=3, Ne4=4"),
    ("1r1r2k1/1p3pp1/pNn1b2p/3p1q2/5B2/P7/1P1Q1PP1/2R1R1K1 b - - 0 1", "d4", "STS(v13.0) Pawn Play in the Center.008", "d4=10, Kh7=1")
    ]

main = do
    args <- getArgs
    case length args of
        0 -> mainPoslist 6	-- default depth 6 takes about 2 minutes
        1 -> mainPoslist (read $ head args)
        _ -> mainDepthFen args

mainPoslist depth = do
    putStrLn $ "Analyse depth " ++ show depth ++ " for all selections of STS"
    ha  <- newCache defaultConfig
    hi  <- newHist
    evs <- makeEvalState Nothing
    TOD s0 ps0 <- getClockTime
    nss <- forM epdList $ \(fen, best, desc, blist) -> do
        let pos = updatePos $ posFromFen fen
        putStr $ unwords . take 2 . words $ fen
        let inist = posToState pos ha hi evs
        n <- searchTheTree 1 depth inist Nothing [] []
        putStrLn $ " (" ++ best ++ ")"
        return n
    TOD s1 ps1 <- getClockTime
    let ttime = fromIntegral (s1 - s0) + fromIntegral (ps1 - ps0) / 10^12
        tnods = sum nss
    putStrLn $ "Total time (secs): " ++ show ttime
    putStrLn $ "Total nodes      : " ++ show tnods
    putStrLn $ "Total nps        : " ++ show (round $ fromIntegral tnods / ttime)

mainDepthFen args = do
    let depth = read $ head args
        fen   = unwords $ tail args
        pos = updatePos $ posFromFen fen
    putStrLn $ "Analyse depth " ++ show depth ++ " fen " ++ fen
    ha  <- newCache defaultConfig
    hi  <- newHist
    evs <- makeEvalState Nothing
    TOD s0 ps0 <- getClockTime
    let inist = posToState pos ha hi evs
    n <- searchTheTree 1 depth inist Nothing [] []
    TOD s1 ps1 <- getClockTime
    let ttime = fromIntegral (s1 - s0) + fromIntegral (ps1 - ps0) / 10^12
    putStrLn ""
    putStrLn $ "Total time (secs): " ++ show ttime
    putStrLn $ "Total nps        : " ++ show (round $ fromIntegral n / ttime)

tellInIO :: Comm -> IO ()
-- tellInIO (LogMes s) = putStrLn $ "Log: " ++ s
-- tellInIO (BestMv a b c d) = putStrLn $ "info score " ++ show a ++ " depth " ++ show b
--                                          ++ " nodes " ++ show c ++ " pv " ++ show d
-- tellInIO (CurrMv a b) = putStrLn $ "info currmove " ++ show a ++ " currmovenumber " ++ show b
-- tellInIO (InfoStr s) = putStrLn s
tellInIO _ = return ()

-- Parameter of the search at this level:
aspirWindow   = 16	-- initial aspiration window
showEvalStats = False	-- show eval statistics in logfile

-- One iteration in the search for the best move
bestMoveCont :: Int -> MyState -> Maybe Int -> [Move] -> [Move] -> IO ([Move], Int, [Move], MyState)
bestMoveCont tiefe stati lastsc lpv rmvs = do
    -- informGuiDepth tiefe
    -- ctxLog "Info" $ "start search for depth " ++ show tiefe
    let abc = ABC {
                maxdepth = tiefe,
                lastpv = lpv,
                lastscore = lastsc,
                rootmvs   = rmvs,
                window    = aspirWindow,
                best      = True,
                stoptime  = 0
                }
    ((sc, path, rmvsf), statf) <- runSearch (alphaBeta abc) stati
    when (sc == 0) $ return ()
    let n = nodes . stats $ statf
    tellInIO (BestMv sc tiefe n path)
    return (path, sc, rmvsf, statf)

searchTheTree :: Int -> Int -> MyState -> Maybe Int -> [Move] -> [Move] -> IO Int
searchTheTree tief mtief mystate lsc lpv rmvs = do
    -- search with the given depth
    (path, sc, rmvsf, stfin) <- bestMoveCont tief mystate lsc lpv rmvs
    case length path of _ -> return () -- because of lazyness!
    if tief >= mtief  -- maximal depth
        then giveBestMove path (nodes $ stats stfin)
        else searchTheTree (tief + 1) mtief stfin (Just sc) path rmvsf

giveBestMove :: [Move] -> Int -> IO Int
giveBestMove mvs nodes = do
    putStr $ case mvs of
        (fmv:_) -> " -> bm " ++ show fmv ++ ", ns " ++ show nodes
        _       -> " -> bm empty PV (" ++ show nodes ++ " nodes)"
    return nodes

-- Opens a parameter file for eval, read it and create an eval state
makeEvalState argfile =
    case argfile of
        Just afn -> do
            fex <- doesFileExist afn
            if fex then filState afn else defState
        Nothing -> defState
    where defState = return $ initEvalState []
          filState fn = fmap initEvalState (fileToParams `fmap` readFile fn)

fileToParams = map readParam . nocomments . lines
    where nocomments = filter (not . iscomment)
          iscomment [] = True
          iscomment ('-':'-':_) = True
          iscomment (c:cs) | isSpace c = iscomment cs
          iscomment _ = False

readParam :: String -> (String, Double)
readParam s = let (ns, vs) = span (/= '=') s in (strip ns, cleanread vs)
    where strip = filter (not . isSpace)
          cleanread = read . tail . strip
