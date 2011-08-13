module Moves.Notation where

import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import Struct.Struct
import Moves.Board

{--
showc :: UArray Int Char
showc = array (0, 15) $ zip [0..] [
             '.', 'P', 'N', 'K', 'x', 'B', 'R', 'Q',
             '.', 'p', 'n', 'k', 'y', 'b', 'r', 'q'
        ]

showLine :: Word8 -> Word8 -> Word8 -> Word8 -> String
showLine w1 w2 w3 w4 = go w1 w2 w3 w4 8 ""
    where go :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> String -> String
          go _ _ _ _ 0 cs = cs
          go x y z t n cs
                   = go (x `shift` 1) (y `shift` 1) (z `shift` 1) (t `shift` 1) (n-1) (c:' ':cs)
               where c = showc ! cap x y z t

cap x y z t = fromIntegral $ (x' .|. shiftR y' 1 .|. shiftR z' 2 .|. shiftR t' 3) `shiftR` 4
    where x' = x .&. 0x80
          y' = y .&. 0x80
          z' = z .&. 0x80
          t' = t .&. 0x80
--}

toNiceNotation :: MyPos -> Move -> String
toNiceNotation p m = piece ++ src ++ capt ++ dst ++ transf ++ check
    where s = fromSquare m
          d = toSquare m
          (sr, sc) = s `divMod` 8
          (dr, dc) = d `divMod` 8
          (fig, fcol) | Busy c f <- tabla p s = (f, c)
          piece = pcToCh fig
          capt | Busy _ _ <- tabla p d = "x"
               | otherwise             = ""
          src = col sc : col sr : ""	-- should be conditional
          dst = col dc : col dr : ""
          transf = if moveIsTransf m then pcToCh (moveTransfPiece m) : [] else []
          p' = doFromToMove m p
          check = if inCheck p' then "+" else ""
          orda = ord 'a'
          ord1 = ord '1'
          col x = chr (orda + x)
          row x = chr (ord1 + x)
          pcToCh King   = "K"
          pcToCh Queen  = "Q"
          pcToCh Rook   = "R"
          pcToCh Bishop = "B"
          pcToCh Knight = "N"
          pcToCh _      = ""

data SrcDest = SDCol Int | SDRow Int | SDColRow Int Int

fromNiceNotation :: MyPos -> Color -> P.Parser Move
fromNiceNotation p c = do
    piece <- parsePiece
    src   <- parseSrcDst
    capt  <- parseCapt
    dst   <- parseSrcDest <|> return src
    tra   <- parseTransf
    chk   <- parseCheck
    let m = moveFromTo ...
    
parsePiece = parseFigure <|> return Pawn

parseFigure = P.oneOf "KQRBN" >>= return . chToPc

chToPc 'K' = King
chToPc 'Q' = Queen
chToPc 'R' = Rook
chToPc 'B' = Bishop
chToPc 'N' = Knight

parseCapt = P.char 'x' >> return True <|> return False

parseSrcDst = parseRow <|> parseColRow

parseRow = P.oneOf "abcdefgh" >>= return . SDRow . (subtract $ ord 'a')

parseColRow = do
    col@(SDCol c) <- parseCol
    parseRow >>= \(SDRow r) -> return (SDColRow c r) <|> return col

parseTransf = P.oneOf "QRBN" >>= return . chToPc

parseCheck = P.char '+' >> return True <|> return False
