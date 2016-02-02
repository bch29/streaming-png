{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Codec.Picture.Png.Streaming.Core
       ( ChunkType
       , ChunkLength
       , PNGDecodeError(..)
       , PNGEncodeError(..)
       , ChunkReader
       , decodeChunk
       , encodeChunk
       ) where

import           Codec.Picture.Png.Streaming.CRC
import           Codec.Picture.Png.Streaming.Util

import           Data.Functor.PullsOver

import           Control.Monad                    (when)
import           Control.Monad.Catch              (Exception, MonadThrow (..))
import           Control.Monad.Trans              (lift)
import qualified Data.ByteString                  as B
import           Data.Int                         (Int64)
import qualified Data.Serialize                   as C
import           Data.Word                        (Word32)

import           Data.ByteString.Streaming        (ByteString)
import qualified Data.ByteString.Streaming        as Q
import           Streaming.Prelude                (Of (..))

type ChunkReader m t = forall x. ByteString m x -> m (t x)

type ChunkType = B.ByteString
type ChunkLength = Word32

data PNGDecodeError
  = IncorrectHeader
  | CRCMismatch
  | UnexpectedEOF
  | BadChunkSize ChunkType
  | UnexpectedChunk ChunkType
  | UnsupportedImageType
  | ExpectedEOF
  deriving (Show)

instance Exception PNGDecodeError where

data PNGEncodeError
  = IncorrectLength
  deriving (Show)

instance Exception PNGEncodeError where

readWord32 :: MonadThrow m => B.ByteString -> m Word32
readWord32 bs =
  case C.runGet C.getWord32be bs of
    Left _ -> throwM UnexpectedEOF
    Right x -> return (fromIntegral x)

checkCRC :: MonadThrow m => Of Word32 (ByteString m r) -> m (ByteString m r)
checkCRC (dataCRC :> bytes) =
  do (crcBS :> bytes') <- Q.toStrict $ Q.splitAt 4 bytes
     crc <- readWord32 crcBS
     if crc == dataCRC
       then return bytes'
       else throwM CRCMismatch

-- | Discard the ByteString in some container, pulling the effects to the
-- outside.
discardInnerBS :: (Monad m, PullsOver m t) => t (ByteString m r) -> m (t r)
discardInnerBS = pullOver . fmap Q.effects

-- | Check the CRC of the ByteString inside some container matches the paired
-- code, pulling the resulting effects to the outside.
checkCRC' :: (MonadThrow m, PullsOver m t) => t (Of Word32 (ByteString m r)) -> m (t (ByteString m r))
checkCRC' = pullOver . fmap checkCRC

-- | Decode a chunk of a PNG file using the provided reader function, wrapping
-- the resulting functor around the rest of the file's data.
decodeChunk
  :: (MonadThrow m, PullsOver m t)
     => (ChunkLength -> ChunkType -> ChunkReader m t)
     -> ByteString m r
     -> m (t (ByteString m r))
decodeChunk reader input =
  do -- The first 4 bytes tell us the length of the chunk
     lenBS :> input' <- Q.toStrict $ Q.splitAt 4 input

     -- Read the length (which is in big endian) using 'cereal',
     -- throwing an error if it can't be read.
     len <- readWord32 lenBS

     -- Split at the end of the chunk, and calculate the CRC of the data, while
     -- copying the chunk data so that we can hand it to 'reader'.
     let input'' = calcCRC32 $ Q.copy $ Q.splitAt (fromIntegral len + 4) input'

     -- The next 4 bytes tell us the chunk type.
     chunkType :> input''' <- Q.toStrict $ Q.splitAt 4 input''

     -- The length of the chunk type can't be more than 4 because we split at 4,
     -- but if it's less then we have encountered an error.
     when (B.length chunkType < 4) (throwM UnexpectedEOF)

     -- Use the provided 'reader' function to read the input, while
     -- simultaneously calculating the CRC of the data.
     readData <- reader len chunkType $ Q.splitAt (fromIntegral len) input'''

     -- Discard the rest of the first ByteString in the result, as it's always
     -- empty, then make sure we fail with an error if the CRC is wrong.
     checkCRC' =<< discardInnerBS readData

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

-- | Encode a chunk of a PNG file with a specified length and chunk type. The
-- input 'ByteString' must be /exactly/ the given length. If it isn't, an
-- 'IncorrectLength' error will be thrown.
encodeChunk
  :: (MonadThrow m)
     => ChunkLength    -- | The length of the /data/ to write, excluding the chunk type.
     -> ChunkType      -- | The 4-character chunk type code, e.g. @"IHDR"@.
     -> ByteString m r -- | The data segment to write.
     -> ByteString m r -- | The resulting data encoded as a PNG chunk.
encodeChunk dataLength chunkType input =
  do let lenBS = C.runPut . C.putWord32be $ dataLength

     -- write the
     Q.fromStrict lenBS

     let -- restrict the length of the input to the given data length as above
         restrictedLen = checkLength (fromIntegral dataLength) input

         -- tack the chunk type bytes to the front
         -- it is important to do this BEFORE calculating the CRC, because the
         -- CRC applies to the chunk type too (but not to the length)
         withChunkType = Q.fromStrict chunkType >> restrictedLen

     -- calculate the CRC and write that to the end
     appendCRC32 withChunkType
