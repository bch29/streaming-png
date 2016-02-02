{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Codec.Picture.Png.Streaming
       (
         -- * Types
         RawHeader
       , RawMetadata
       , RawData
       , RawPNG
       , LabelledByteString(..)
       , PNGDecodeError
       , PNGEncodeError
       , HeaderData

         -- * Decoding
       , decodePNG
       , decodePNGComplete

         -- * Encoding
       , encodePNG
       )
       where

import           Codec.Picture.Png.Streaming.ChunkReaders
import           Codec.Picture.Png.Streaming.Core
import           Codec.Picture.Png.Streaming.Util

import           Control.Monad                            (ap, when, join)
import           Control.Monad.Catch                      (MonadThrow (..))
import           Control.Monad.Morph                      (hoist)
import           Control.Monad.Trans                      (MonadTrans (..))
import           Data.Functor.Identity                    (Identity (..))
import           Data.Functor.Sum                         (Sum (..))

import qualified Data.ByteString                          as B
import           Data.ByteString.Streaming                (ByteString)
import qualified Data.ByteString.Streaming                as Q
import qualified Streaming                                as S
import           Streaming.Prelude                        (Of (..), Stream)

-- | A streaming representation of a raw PNG header. A promise in a monad @m@
-- that we will get the header followed by some return value.
type RawHeader m r = m (Of HeaderData r)

-- | A streaming representation of raw PNG metadata. An effectful stream of
-- binary chunks labelled with their types, followed by some return value.
type RawMetadata m r = Stream (LabelledByteString (ChunkLength, ChunkType) m) m r

-- | A streaming representation of raw PNG data. An effectful stream of
-- binary chunks labelled with their lengths, followed by some return value.
type RawData m r = Stream (LabelledByteString ChunkLength m) m r

-- | A streaming representation of a raw PNG file. An effectful 3-tuple of a
-- header, then some metadata, then some raw data, followed by some return
-- value.
type RawPNG m r = RawHeader m (RawMetadata m (RawData m r))

-- | Fully decode a PNG to its raw streaming representation, returning the PNG
-- data followed by anything remaining in the data source.
decodePNG :: (MonadThrow m) => ByteString m r -> RawPNG m (ByteString m r)
decodePNG = fmap (fmap (fmap decodeDataWithInitial . decodeMetadata)) . decodeHeader . checkSig
  where
    decodeDataWithInitial input =
      do remainder <- yieldOrIgnore input
         decodeData remainder

    -- This function ignores empty inputs and yields all others. Avoids
    -- polluting the result with an empty data chunk if there is no data.
    yieldOrIgnore (LabelledByteString 0 res) = lift (Q.effects res)
    yieldOrIgnore input = S.yields input

-- | Fully code a PNG to its raw streaming representation, returning the PNG
-- data followed by the return value of the effectful source. Throws an error
-- if there is any more data remaining in the source after the PNG.
decodePNGComplete :: (MonadThrow m) => ByteString m r -> RawPNG m r
decodePNGComplete = fmap (fmap (fmap (>>= lift . expectNull ExpectedEOF))) . decodePNG

-- | Encode a raw PNG as a streaming ByteString.
encodePNG :: (MonadThrow m) => RawPNG m r -> ByteString m r
encodePNG png =
  do writeSig
     metadata <- encodeHeader png
     mainData <- encodeMetadata metadata
     res <- encodeData mainData
     writeIEND
     return res

--------------------------------------------------------------------------------
-- Internals

-- | The 8-byte signature for a PNG file.
pngSignature :: B.ByteString
pngSignature = "\137PNG\r\n\26\n"

--------------------------------------------------------------------------------
-- Decoding

-- | Check the fixed PNG signature, return the rest of the input after the signature.
checkSig :: MonadThrow m => ByteString m r -> ByteString m r
checkSig input = Q.mwrap $ do
  (header :> remainder) <- Q.toStrict $ Q.splitAt 8 input
  if header == pngSignature
    then return remainder
    else throwM IncorrectHeader

-- | Decode a PNG header from a stream, returning the header followed by the
-- rest of the data.
decodeHeader :: MonadThrow m => ByteString m r -> RawHeader m (ByteString m r)
decodeHeader = decodeChunk reader
  where
    reader _ chunkType
      | chunkType == "IHDR" = chunkReaderIHDR
      | otherwise = chunkReaderError (UnexpectedChunk chunkType)

-- | Decode PNG metadata from a stream, returning the header followed by the
-- first (possibly empty) chunk of main data, followed by the rest of the data.
decodeMetadata :: MonadThrow m => ByteString m r -> RawMetadata m (LabelledByteString ChunkLength m (ByteString m r))
decodeMetadata input =
  do res <- lift $ decodeChunk metadataReader input
     case res of
       EMDEnd remainder -> LabelledByteString 0 <$> return (return remainder)
       EMDData cl dataRemainder -> return (LabelledByteString cl dataRemainder)
       EMDMetadata cl ct metadataRemainder ->
         do remainder <- S.yields (LabelledByteString (cl, ct) metadataRemainder)
            decodeMetadata remainder

-- | Decode PNG data chunks from a stream, returning the data followed by
-- anything remaining in the data source.
decodeData :: MonadThrow m => ByteString m r -> RawData m (ByteString m r)
decodeData input =
  do res <- lift $ decodeChunk dataReader input
     case res of
       EODEnd remainder -> return remainder
       EODData cl dataRemainder ->
         do remainder <- S.yields (LabelledByteString cl dataRemainder)
            decodeData remainder

--------------------------------------------------------------------------------
-- Encoding

writeSig :: Monad m => ByteString m ()
writeSig = Q.fromStrict pngSignature

encodeHeader :: (MonadThrow m) => RawHeader m r -> ByteString m r
encodeHeader hdr =
  do (dat :> res) <- lift hdr
     encodeChunk 13 "IHDR" $ Q.fromStrict (serializeHeaderData dat)
     return res

encodeMetadata :: (MonadThrow m) => RawMetadata m r -> ByteString m r
encodeMetadata = S.iterTM step
  where step (LabelledByteString (cl, ct) metadata) = join $ encodeChunk cl ct metadata

encodeData :: (MonadThrow m) => RawData m r -> ByteString m r
encodeData = S.iterTM step
  where step (LabelledByteString cl dat) = join $ encodeChunk cl "IDAT" dat

writeIEND :: (MonadThrow m) => ByteString m ()
writeIEND = encodeChunk 0 "IEND" mempty
