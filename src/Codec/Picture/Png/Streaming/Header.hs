{-# LANGUAGE RecordWildCards #-}
module Codec.Picture.Png.Streaming.Header
       ( HeaderData(..)
       , takeHeaderData
       , tryDecodeHeader
       )
       where

import           Codec.Picture.Png.Streaming.Core
import           Codec.Picture.Png.Streaming.Info
import           Codec.Picture.Png.Streaming.Util

import           Control.Monad (unless)
import           Control.Monad.Catch              (MonadThrow (..))

import qualified Data.ByteString                  as B
import qualified Data.Serialize                   as C
import           Data.Word                        (Word8, Word32)

import           Data.ByteString.Streaming        (ByteString)
import qualified Data.ByteString.Streaming        as Q
import           Streaming                        (Stream)
import qualified Streaming                        as S
import           Streaming.Prelude                (Of (..))

-- | Represents the header data of a PNG file in raw binary format.
data HeaderData =
  HeaderData
  { hdWidth             :: Word32
  , hdHeight            :: Word32
  , hdBitDepth          :: BitDepth
  , hdColourType        :: ColourType
  , hdCompressionMethod :: Word8
  , hdFilterMethod      :: Word8
  , hdInterlaceMethod   :: Word8
  }
  deriving (Show)

-- | Take the header data from the front of a stream and return the remainder.
-- Throws an 'UnexpectedChunk' exception if the first chunk isn't of type
-- "IHDR", or 'UnexpectedEOF' if the stream is empty.
takeHeaderData :: (MonadThrow m) => Stream (PNGChunk m) m r -> m (Of HeaderData (Stream (PNGChunk m) m r))
takeHeaderData input =
  do maybeChunk <- S.inspect input
     case maybeChunk of
       Left _ -> throwM UnexpectedEOF
       Right PNGChunk{..} ->
         do unless (chunkType == ctIHDR) (throwM (UnexpectedChunk chunkType))
            tryDecodeHeader chunkData

deserializeHeaderData :: B.ByteString -> Either String HeaderData
deserializeHeaderData = C.runGet $
  do hdWidth             <- C.getWord32be
     hdHeight            <- C.getWord32be
     hdBitDepth          <- C.getWord8
     hdColourType        <- C.getWord8
     hdCompressionMethod <- C.getWord8
     hdFilterMethod      <- C.getWord8
     hdInterlaceMethod   <- C.getWord8
     return HeaderData{..}

serializeHeaderData :: HeaderData -> B.ByteString
serializeHeaderData HeaderData{..} = C.runPut $
  do C.putWord32be hdWidth
     C.putWord32be hdHeight
     C.putWord8 hdBitDepth
     C.putWord8 hdColourType
     C.putWord8 hdCompressionMethod
     C.putWord8 hdFilterMethod
     C.putWord8 hdInterlaceMethod

-- | Try to decode a PNG header from a ByteString, failing if it is of the wrong
-- length.
tryDecodeHeader
  :: MonadThrow m => ByteString m r -> m (Of HeaderData r)
tryDecodeHeader input =
  do (bytes :> rest) <- Q.toStrict $ Q.splitAt (fromIntegral ihdrLength) input
     res <- expectNull (BadChunkSize ctIHDR) rest
     parsed <- either (throwM . UnknownError) return (deserializeHeaderData bytes)
     return (parsed :> res)
