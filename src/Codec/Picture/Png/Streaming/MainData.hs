{-|
Module : Codec.Picture.Png.Streaming.MainData
Copyright : (c) Bradley Hardy 2016
License: LGPL3
Maintainer: bradleyhardy@live.com
Stability: experimental
Portability: portable

-}
{-# LANGUAGE RecordWildCards #-}
module Codec.Picture.Png.Streaming.MainData
       ( decodeImageData )
       where

import           Codec.Picture.Png.Streaming.Core
import           Codec.Picture.Png.Streaming.Header
import           Codec.Picture.Png.Streaming.Info
import           Codec.Picture.Png.Streaming.Util
import           Streaming.Zlib

import           Control.Monad.Catch                (MonadThrow (..))
import           Control.Monad.IO.Class             (MonadIO (..))

import qualified Data.ByteString                    as B
import qualified Data.ByteString.Unsafe             as B
import           Data.Functor.Identity              (Identity (..))
import           Data.Functor.Sum                   (Sum (..))
import           Data.Int                           (Int64)
import qualified Data.Vector.Storable               as Vec
import qualified Data.Vector.Storable.Mutable       as Vec
import           Data.Word                          (Word64)

import           Data.ByteString.Streaming          (ByteString)
import qualified Data.ByteString.Streaming          as Q
import           Streaming                          (Of (..), Stream)
import qualified Streaming                          as S

--------------------------------------------------------------------------------
-- Main function

{-|
Given a stream of PNG chunks, decode the image data from it. The resulting bytes
can be directly interpreted as pixels, whose format depends on the image's
colour type.
-}
decodeImageData
  :: (MonadThrow m, MonadIO m)
     => HeaderData
     -> Stream (PNGChunk m) m r
     -> ByteString m r
decodeImageData hd@HeaderData{..} =
  let prevByteDistance =
        case getBitsPerPixel hdBitDepth hdColourType of
          Just bpp -> (fromIntegral bpp + 4) `div` 8
          Nothing -> 1
  in Q.fromChunks
    . reconstructScanlines prevByteDistance
    . splitImageDataByScanlines hd
    . getDecompressedImageData

--------------------------------------------------------------------------------
-- Internals

{- |

Given a chunk type and a mapping function from a streaming ByteString to some
other functor, returns a filtering function suitable for passing to
'filterMapped' which accepts only the specified chunk type and applies the given
function to the bytes of the chunks of that type.

-}
mapChunkOfType
  :: (Monad m, Functor f)
     => ChunkType
     -> (ByteString m r -> m (f r))
     -> PNGChunk m r
     -> m (Sum Identity f r)
mapChunkOfType desiredType f PNGChunk{..}
  | chunkType == desiredType = InR <$> f chunkData
  | otherwise = InL . Identity <$> Q.effects chunkData

-- | From a raw stream of PNG chunks, filter out the IDAT chunks and decompress
-- them.
getDecompressedImageData
  :: (MonadIO m, MonadThrow m)
     => Stream (PNGChunk m) m r
     -> ByteString m r
getDecompressedImageData
  = decompressStream
  . Q.concat
  . filterMapped (mapChunkOfType ctIDAT return)

-- | Returns the number of bytes per scanline in a PNG with the given header, so
-- long as the colour type and bit depth fields are compatible with each other.
getScanlineLengthBytes :: HeaderData -> Maybe Int64
getScanlineLengthBytes HeaderData{..}
  = (1 +) -- Each scanline is preceded by a filter type byte
  . ceiling
  . (/ (8.0 :: Float))
  . fromIntegral
  . (* (fromIntegral hdWidth :: Word64))
  . fromIntegral
  <$> getBitsPerPixel hdBitDepth hdColourType

-- | Split a stream of raw, decompressed, PNG image data into scanlines.
splitImageDataByScanlines
  :: (Monad m, MonadThrow m)
     => HeaderData
     -> ByteString m r
     -> Stream (ByteString m) m r
splitImageDataByScanlines hd@HeaderData{..} input = S.effect $
  do scanlineLength <-
       maybe (throwM UnsupportedColourType) return (getScanlineLengthBytes hd)

     return (chunksOfBS scanlineLength input)

type UnfilteredScanline = B.ByteString
type FilteredScanline = B.ByteString

-- | Given a stream of filtered scanlines, reconstruct each of them.
reconstructScanlines
  :: (MonadThrow m, MonadIO m)
     => Int
     -> Stream (ByteString m) m r
     -> Stream (Of UnfilteredScanline) m r
reconstructScanlines prevByteDistance
  = mapWithMemory (reconstructScanline prevByteDistance)
  . S.mapped Q.toStrict

{- |

Given a filtered scanline (whose first byte encodes the filter method), along
with the previous unfiltered scanline (which is 'Nothing' if we're at the first
scanline), reconstruct the pixel data. Fails at runtime, possible even with a
segfault, if either input scanline is the wrong size.

This is a giant ugly mess, but it's fast. Can it be refactored into something
nicer while retaining its speed?

Also, it still takes up roughly 50% of the running time of decoding a PNG. Maybe
it can be made even faster?
-}
reconstructScanline
  :: (MonadThrow m, MonadIO m)
     => Int
     -> Maybe UnfilteredScanline
     -> FilteredScanline
     -> m B.ByteString
reconstructScanline prevByteDistance mprev filteredLine
  | Just (filterType, this) <- B.uncons filteredLine =
      let lenThis = B.length this

          -- This function is equivalent to @forM_ [0 .. lenThis - 1]@, but
          -- slightly faster in my benchmarks
          loop :: Monad m => (Int -> m ()) -> m ()
          loop action = go 0
            where
              go n | n < lenThis = do action n; go (n + 1)
                   | otherwise = return ()
          {-# INLINE loop #-}

          noFilter = this

          subFilter = vectorToBytestring $ Vec.create $
            do v <- Vec.new lenThis
               loop $ \i ->
                 do a <- if i >= prevByteDistance
                         then Vec.read v (i - prevByteDistance)
                         else return 0
                    Vec.write v i (B.unsafeIndex this i + a)
               return v

          upFilter =
            case mprev of
              Just prev ->
                vectorToBytestring $ Vec.generate lenThis $ \i ->
                  B.unsafeIndex prev i + B.unsafeIndex this i
              Nothing -> this

          averageFilter = vectorToBytestring $ Vec.create $
            case mprev of
              Just prev ->
                do v <- Vec.new lenThis
                   loop $ \i ->
                     do a <- if i >= prevByteDistance
                             then Vec.read v (i - prevByteDistance)
                             else return 0
                        Vec.write v i (reconAverage a (B.unsafeIndex prev i) (B.unsafeIndex this i))
                   return v
              Nothing ->
                do v <- Vec.new lenThis
                   loop $ \i ->
                     do a <- if i >= prevByteDistance
                             then Vec.read v (i - prevByteDistance)
                             else return 0
                        Vec.write v i (reconAverage a 0 (B.unsafeIndex this i))
                   return v

          paethFilter = vectorToBytestring $ Vec.create $
            case mprev of
              Just prev ->
                do v <- Vec.new lenThis
                   loop $ \i ->
                     do (a, c) <- if i >= prevByteDistance
                                  then do a <- Vec.read v (i - prevByteDistance)
                                          return (a, B.unsafeIndex prev (i - prevByteDistance))
                                  else return (0, 0)
                        Vec.write v i (reconPaeth a (B.unsafeIndex prev i) c (B.unsafeIndex this i))
                   return v
              Nothing ->
                do v <- Vec.new lenThis
                   loop $ \i ->
                     do a <- if i >= prevByteDistance
                             then Vec.read v (i - prevByteDistance)
                             else return 0
                        Vec.write v i (reconPaeth a 0 0 (B.unsafeIndex this i))
                   return v

          res | filterType == 0 = noFilter
              | filterType == 1 = subFilter
              | filterType == 2 = upFilter
              | filterType == 3 = averageFilter
              | filterType == 4 = paethFilter
              | otherwise = mempty

         in if B.length res == lenThis
            then return res
            else throwM (UnsupportedFilterType filterType)

  | otherwise = error "reconstructScanline: empty input"
