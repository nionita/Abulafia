module Eval.BasicEval (
    matPiece
) where

import Struct.Struct

matPiece1 :: Piece -> Int
matPiece1 Pawn   = 100
matPiece1 Knight = 325
matPiece1 Bishop = 325
matPiece1 Rook   = 500
matPiece1 Queen  = 975
matPiece1 King   = 20000

matPiece :: Color -> Piece -> Int
matPiece c p = case c of
               White ->   matPiece1 p
               Black -> - matPiece1 p
