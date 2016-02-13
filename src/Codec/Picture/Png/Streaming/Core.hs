{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module Codec.Picture.Png.Streaming.Core
       ( ChunkType
       , ChunkLength
       , PNGDecodeError(..)
       , PNGEncodeError(..)
       , PNGChunk(..)
       , decodePNGChunks
       , encodePNGChunks
       , decodeChunk
       )
       where

import           Streaming.CRC
import           Codec.Picture.Png.Streaming.Info
import           Codec.Picture.Png.Streaming.Util

import           Control.Monad                    (join, when)
import           Control.Monad.Catch              (Exception, MonadThrow (..))
import           Control.Monad.Trans              (lift)
import qualified Data.ByteString                  as B
import           Data.Int                         (Int64)
import qualified Data.Serialize                   as C
import           Data.Word                        (Word32)

import           Data.ByteString.Streaming        (ByteString)
import qualified Data.ByteString.Streaming        as Q
import           Streaming                        (Stream)
import qualified Streaming                        as S
import           Streaming.Prelude                (Of (..))

--------------------------------------------------------------------------------
-- Types

-- | A raw chunk of PNG data, containing the length, type and a streaming
-- 'ByteString' of data.
data PNGChunk m r =
  PNGChunk
  { chunkLength :: ChunkLength
  , chunkType   :: ChunkType
  , chunkData   :: ByteString m r
  }
  deriving (Functor)

-- | The type of errors that might arise when decoding a PNG.
data PNGDecodeError
  = IncorrectSignature
  | CRCMismatch
  | UnexpectedEOF
  | BadChunkSize ChunkType
  | UnexpectedChunk ChunkType
  | UnsupportedImageType
  | UnsupportedColourType
  | UnsupportedFilterType FilterType
  | ExpectedEOF
  | UnknownError String
  deriving (Show)

instance Exception PNGDecodeError where

-- | The type of errors that might arise when encoding a PNG.
data PNGEncodeError
  = IncorrectLength
  deriving (Show)

instance Exception PNGEncodeError where

--------------------------------------------------------------------------------
-- Raw decode and encode functions

-- | Fully decode a PNG to its raw streaming representation, returning the PNG
-- data chunks until "IEND" is reached. The return value is the (potentially
-- empty) rest of the 'ByteString' after the PNG.

{-|

Decode a stream of individual PNG chunks from a raw streaming 'ByteString'
input. The return value of the resulting stream is any remaining data after
encountering the final "IEND" chunk.

-}
decodePNGChunks :: (MonadThrow m) => ByteString m r -> Stream (PNGChunk m) m (ByteString m r)
decodePNGChunks = S.unfold decodeChunk . checkSig

-- | Encode a stream of individual PNG chunks into a raw streaming 'ByteString'.
encodePNGChunks :: (MonadThrow m) => Stream (PNGChunk m) m r -> ByteString m r
encodePNGChunks png =
  do Q.fromStrict pngSignature
     res <- S.iterTM (join . encodeChunk) png
     encodeChunk chunkIEND
     return res

--------------------------------------------------------------------------------
-- Decoding and encoding individual chunks

-- | Decode a chunk of a PNG file, returning the raw binary data in the chunk
-- followed by the rest of the input; or the remaining data in the 'ByteString'
-- if we have reached the "IEND" chunk.
decodeChunk
  :: (MonadThrow m)
     => ByteString m r
     -> m (Either (ByteString m r) (PNGChunk m (ByteString m r)))
decodeChunk input =
  do -- The first 4 bytes tell us the length of the chunk
     lenBS :> input' <- Q.toStrict $ Q.splitAt 4 input

     -- Read the length (which is in big endian) using 'cereal',
     -- throwing an error if it can't be read.
     chunkLength <- readWord32 lenBS

     -- Split at the end of the chunk, and calculate the CRC of the data, while
     -- copying the chunk data so that we can hand it to 'reader'.
     let input'' = calcCRC32 $ Q.copy $ Q.splitAt (fromIntegral chunkLength + 4) input'

     -- The next 4 bytes tell us the chunk type.
     chunkType :> input''' <- Q.toStrict $ Q.splitAt 4 input''

     -- The length of the chunk type can't be more than 4 because we split at 4,
     -- but if it's less then we have encountered an error.
     when (B.length chunkType < 4) (throwM UnexpectedEOF)

     -- Use the provided 'reader' function to read the input, while
     -- simultaneously calculating the CRC of the data.
     let readData = Q.splitAt (fromIntegral chunkLength) input'''

     -- Discard the rest of the first ByteString in the result, as it's always
     -- empty, then make sure we fail with an error if the CRC is wrong.
         chunkData = checkCRC <$> Q.drained readData

     if chunkType /= ctIEND
       then return $ Right PNGChunk{..}
       else Left <$> Q.effects chunkData

-- | Encode a chunk of a PNG file with a specified length and chunk type. The
-- input 'PNGChunk' must not lie about its length. If it does, an
-- 'IncorrectLength' error will be thrown. It is important to know the length
-- at the head of the chunk to maintain streaming.
encodeChunk
  :: (MonadThrow m)
     => PNGChunk m r   -- | The data segment to write.
     -> ByteString m r -- | The resulting data encoded as a PNG chunk.
encodeChunk PNGChunk{..} =
  do let lenBS = C.runPut . C.putWord32be $ chunkLength

     -- write the
     Q.fromStrict lenBS

     let -- restrict the length of the input to the given data length as above
         restrictedLen = checkLength (fromIntegral chunkLength) chunkData

         -- tack the chunk type bytes to the front
         -- it is important to do this BEFORE calculating the CRC, because the
         -- CRC applies to the chunk type too (but not to the length)
         withChunkType = Q.fromStrict chunkType >> restrictedLen

     -- calculate the CRC and write that to the end
     appendCRC32 withChunkType

--------------------------------------------------------------------------------
-- Extra helper functions

-- | Check the length of a ByteString against an expected length. Running the
-- resulting 'ByteString' to the end will throw an error if it is not exactly the
-- right length.
checkLength
  :: MonadThrow m
     => Int64
     -> ByteString m r
     -> ByteString m r
checkLength expectedLength input =
  do (len :> rest) <- Q.length $ Q.copy $ Q.splitAt expectedLength input
     when (fromIntegral len /= expectedLength) $
       lift $ throwM IncorrectLength

     lift $ expectNull IncorrectLength rest

-- | Read 32 bits from a 'B.ByteString' into a 'Word32'. Throws an
-- 'UnexpectedEOF' exception if the bits can't be read.
readWord32 :: MonadThrow m => B.ByteString -> m Word32
readWord32 bs =
  case C.runGet C.getWord32be bs of
    Left _ -> throwM UnexpectedEOF
    Right x -> return (fromIntegral x)

-- | Given an expected CRC and a 'ByteString' whose first 4 bytes should match
-- that CRC, check if there is a match. If not, throw a 'CRCMismatch' error. If
-- it matches, return the remainder of the 'ByteString'.
checkCRC :: MonadThrow m => Of Word32 (ByteString m r) -> ByteString m r
checkCRC (dataCRC :> bytes) = Q.mwrap $
  do (crcBS :> bytes') <- Q.toStrict $ Q.splitAt 4 bytes
     crc <- readWord32 crcBS
     if crc == dataCRC
       then return bytes'
       else throwM CRCMismatch

-- | Check the fixed PNG signature, return the rest of the input after the
-- signature.
checkSig :: MonadThrow m => ByteString m r -> ByteString m r
checkSig input = Q.mwrap $ do
  (header :> remainder) <- Q.toStrict $ Q.splitAt 8 input
  if header == pngSignature
    then return remainder
    else throwM IncorrectSignature

chunkIEND :: (Monad m) => PNGChunk m ()
chunkIEND =
  let chunkLength = 0
      chunkType = ctIEND
      chunkData = mempty
  in PNGChunk{..}
