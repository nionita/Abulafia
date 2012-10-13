module Moves.Notation where

import Data.Array.Unboxed
import Data.Bits
import Data.List
import Data.Word
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec ((<|>))

import Struct.Struct
import Moves.Moves
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
    where piece = pcToCh fig
          s = fromSquare m
          d = toSquare m
          (sr, sc) = s `divMod` 8
          (dr, dc) = d `divMod` 8
          (fig, fcol) | Busy c f <- tabla p s = (f, c)
          capt | Busy _ _ <- tabla p d = "x"
               | otherwise             = ""
          att = fAttacs d fig (occup p)
          our = if fcol == White then white p else black p
          src | fig == Pawn = col sc : ""
              | fig == King = ""
              | fig == Knight = desamb (knights p)
              | fig == Bishop = desamb (bishops p)
              | fig == Rook   = desamb (rooks p)
              | fig == Queen  = desamb (queens p)
          dst = col dc : row dr : ""
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
          desamb b
              | popCount1 b0 == 1 = ""
              | popCount1 (b0 .&. colBB sc) == 1 = col sc : ""
              | popCount1 (b0 .&. rowBB sr) == 1 = row sr : ""
              | otherwise         = col sc : row sr : ""
              where b0 = b .&. att .&. our

data SrcDest = SDCol Int | SDRow Int | SDColRow Int Int

fromNiceNotation :: MyPos -> Color -> P.Parser Move
fromNiceNotation p c = do
    piece <- parsePiece
    srcc  <- parseSrcOrCapt
    (msrc, capt, dst) <- case srcc of
        Left src -> do
            capt <- parseCapt
            if capt
               then do
                   dst  <- parseSrcDst
                   return (Just src, capt, dst)
               else do
                   mdst <- parseSrcDst >>= return . Just <|> return Nothing
                   case mdst of
                       Just dst -> return (Just src, capt, dst)
                       Nothing  -> return (Nothing,  capt, src)
        Right capt -> do
            dst <- parseSrcDst
            return (Nothing, capt, dst)
    tra <- parseTransf
    chk <- parseCheck
    case msrc of
        Just (SDCol x) ->
        Just (SDRow y) ->
        Just (SDColRow x y) ->
        Nothing        ->
    let m = moveFromTo ...
    
parsePiece = parseFigure <|> return Pawn

parseFigure = P.oneOf "KQRBN" >>= return . chToPc

chToPc 'K' = King
chToPc 'Q' = Queen
chToPc 'R' = Rook
chToPc 'B' = Bishop
chToPc 'N' = Knight

parseSrcOrCapt = P.char 'x' >> return (Right True)
             <|> parseSrcDst >>= return . Left

parseCapt = P.char 'x' >> return True <|> return False

parseSrcDst = parseRow <|> parseColRow

parseCol = P.oneOf "abcdefgh" >>= return . SDCol . (subtract $ ord 'a')

parseRow = P.oneOf "12345678" >>= return . SDRow . (subtract $ ord '1')

parseColRow = do
    col@(SDCol c) <- parseCol
    parseRow >>= \(SDRow r) -> return (SDColRow c r) <|> return col

parseTransf = P.oneOf "QRBN" >>= return . chToPc

parseCheck = P.char '+' >> return True <|> return False
