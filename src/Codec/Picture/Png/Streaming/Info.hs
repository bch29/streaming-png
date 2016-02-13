{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.Picture.Png.Streaming.Info where

import qualified Data.ByteString as B
import           Data.Int        (Int16)
import           Data.Word       (Word16, Word32, Word8)

type ChunkType = B.ByteString
type ChunkLength = Word32
type ColourType = Word8
type BitDepth = Word8
type CompressionMethod = Word8
type FilterMethod = Word8
type InterlaceMethod = Word8

ctIHDR, ctIDAT, ctIEND :: ChunkType
ctIHDR = "IHDR"
ctIDAT = "IDAT"
ctIEND = "IEND"

-- | The 8-byte signature for a PNG file.
pngSignature :: B.ByteString
pngSignature = "\137PNG\r\n\26\n"

-- | The length of the header chunk.
ihdrLength :: ChunkLength
ihdrLength = 13

-- | From a bit depth and colour type, return the number of bits in each pixel,
-- checking also that the bit depth provided is allowed for the given colour
-- type.
getBitsPerPixel :: BitDepth -> ColourType -> Maybe Word8
getBitsPerPixel bitDepth colourType
    -- greyscale
  | colourType == 0 && bitDepth `elem` [1, 2, 4, 8, 16] = Just bitDepth
    -- indexed colour
  | colourType == 3 && bitDepth `elem` [1, 2, 4, 8]     = Just bitDepth
    -- truecolour
  | colourType == 2 && bitDepth == 8 || bitDepth == 16  = Just (bitDepth * 3)
    -- greyscale with alpha
  | colourType == 4 && bitDepth == 8 || bitDepth == 16  = Just (bitDepth * 2)
    -- truecolour with alpha
  | colourType == 6 && bitDepth == 8 || bitDepth == 16  = Just (bitDepth * 4)
    -- unknown
  | otherwise = Nothing

type FilterType = Word8

-- | The Paeth predictor function due to Alan W. Paeth.
paethPredictor :: Word8 -> Word8 -> Word8 -> Word8
paethPredictor a b c =
  -- It is important to convert the bytes to signed integers before doing the
  -- calculations, because the Paeth predictor relies on signed arithmetic.
  let ia = fromIntegral a :: Int16
      ib = fromIntegral b
      ic = fromIntegral c

      p = ia + ib - ic
      pa = abs (p - ia)
      pb = abs (p - ib)
      pc = abs (p - ic)
  in if | pa <= pb && pa <= pc -> a
        | pb <= pc -> b
        | otherwise -> c

{- |
Given a filter type, return the reconstruction function for a single byte, if
the filter type is valid. If @f@ is a function returned by 'getReconFunction',
the correct argument order is @f a b c x@, where those variable names match the
ones used in the <http://www.w3.org/TR/PNG/#9Filter-types PNG specification,
section 9.2>.
-}
getReconFunction :: FilterType -> Maybe (Word8 -> Word8 -> Word8 -> Word8 -> Word8)
getReconFunction filterType =
  let ftInt = fromIntegral filterType
      funs = [ reconNone
             , reconSub
             , reconUp
             , reconAverage
             , reconPaeth ]

      reconNone _ _ _ x = x
      reconSub a _ _ x = x + a
      reconUp _ b _ x = x + b
      reconAverage a b _ x = x + fromIntegral ((fromIntegral a + fromIntegral b) `div` (2 :: Word16))
      reconPaeth a b c x = x + paethPredictor a b c

  in if ftInt < length funs
     then Just (funs !! ftInt)
     else Nothing
