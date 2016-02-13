{-|
Module : Codec.Picture.Png.Streaming.Info
Copyright : (c) Bradley Hardy 2016
License: LGPL3
Maintainer: bradleyhardy@live.com
Stability: experimental
Portability: portable

-}

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
module Codec.Picture.Png.Streaming.Info where

import qualified Data.ByteString as B
import           Data.Int        (Int16)
import           Data.Word       (Word16, Word32, Word8)

-- | A chunk type, represented as a 'B.ByteString' of length 4.
type ChunkType = B.ByteString

-- | A chunk length.
type ChunkLength = Word32

-- | A colour type.
type ColourType = Word8

-- | A bit depth.
type BitDepth = Word8

-- | A PNG compression method (distinct from the type in 'Streaming.Zlib' of the
-- same name).
type CompressionMethod = Word8

-- | A PNG filtering method.
type FilterMethod = Word8

-- | A PNG interlacing method.
type InterlaceMethod = Word8

-- | A PNG filter type, for filter method 0.
type FilterType = Word8

-- | The 4-byte identifier for a PNG header chunk.
ctIHDR :: ChunkType
ctIHDR = "IHDR"

-- | The 4-byte identifier for a PNG data chunk.
ctIDAT :: ChunkType
ctIDAT = "IDAT"

-- | The 4-byte identifier for a PNG ending chunk.
ctIEND :: ChunkType
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

reconNone :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
reconNone _ _ _ x = x
{-# INLINE reconNone #-}

reconSub :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
reconSub a _ _ x = x + a
{-# INLINE reconSub #-}

reconUp :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
reconUp _ b _ x = x + b
{-# INLINE reconUp #-}

reconAverage :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
reconAverage a b _ x = x + fromIntegral ((fromIntegral a + fromIntegral b) `div` (2 :: Word16))
{-# INLINE reconAverage #-}

reconPaeth :: Word8 -> Word8 -> Word8 -> Word8 -> Word8
reconPaeth a b c x = x + paethPredictor a b c
{-# INLINE reconPaeth #-}

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

  in if ftInt < length funs
     then Just (funs !! ftInt)
     else Nothing
