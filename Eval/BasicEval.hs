module Eval.BasicEval (
    matPiece
) where

import Data.Array.IArray
import Data.Bits
import Data.List

import Struct.Struct
-- import Moves
-- import BitBoard
-- import Board

-- Values for material
mvals :: Array (Color, Piece) Int
mvals = array ((White, Pawn), (Black, King)) [
           ((White, Pawn), 100), ((Black, Pawn), -100),
           ((White, Knight), 325), ((Black, Knight), -325),
           ((White, Bishop), 325), ((Black, Bishop), -325),
           ((White, Rook), 500), ((Black, Rook), -500),
           ((White, Queen), 975), ((Black, Queen), -975),
           ((White, King), 20000), ((Black, King), -20000)
           ]

-- matPiece :: Color -> Piece -> Int
-- matPiece c p = mvals ! (c, p)

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
