{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
module Codec.Picture.Png.Streaming.ChunkReaders where

import           Codec.Picture.Png.Streaming.Core

import           Data.Functor.PullsOver

import           Control.Monad.Catch              (MonadThrow (..))
import qualified Data.ByteString                  as B
import           Data.Functor.Identity            (Identity (..))
import           Data.Functor.Sum                 (Sum (..))
import qualified Data.Serialize                   as C
import           Data.Word                        (Word32, Word8)

import           Data.ByteString.Streaming        (ByteString)
import qualified Data.ByteString.Streaming        as Q
import           Streaming.Prelude                (Of (..))

type Pixel = Word32

-- | Represents the header data of a PNG file in raw binary format.
data HeaderData =
  HeaderData
  { hdWidth             :: Word32
  , hdHeight            :: Word32
  , hdBitDepth          :: Word8
  , hdColourType        :: Word8
  , hdCompressionMethod :: Word8
  , hdFilterMethod      :: Word8
  , hdInterlaceMethod   :: Word8
  }
  deriving (Show)

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

-- | A streaming 'ByteString' labelled with some extra (strict) data. Isomorphic
-- to @'Compose' ('Of' t) ('ByteString' m)@, but more descriptive.
data LabelledByteString t m r =
  LabelledByteString
  { lbsLabel          :: !t
  , unlabelByteString :: ByteString m r
  }
  deriving (Functor)

-- | Reader for PNG header information. Returns the 'HeaderData' paired with the
-- rest of the data.
chunkReaderIHDR :: MonadThrow m => ChunkReader m (Of HeaderData)
chunkReaderIHDR bs = do
  bytes :> bs' <- Q.toStrict $ Q.splitAt 13 bs

  case deserializeHeaderData bytes of
    Left _ -> throwM UnexpectedEOF
    Right hdr -> do
      isEmpty <- Q.null_ bs'
      if isEmpty
        then fmap (hdr :>) (Q.effects bs')
        else throwM (BadChunkSize "IHDR")

-- | Reader that ignores its input and throws an error.
chunkReaderError :: (MonadThrow m) => PNGDecodeError -> ChunkReader m t
chunkReaderError err _ = throwM err

-- | Reader that skips past its inputs and returns the following data in an
-- arbitrary applicative.
chunkReaderIgnore :: (MonadThrow m, Applicative f) => ChunkReader m f
chunkReaderIgnore = fmap pure . Q.effects

-- | Reader that just lets the bytes pass through and returns them as-is.
chunkReaderPassThrough :: (MonadThrow m) => ChunkReader m (ByteString m)
chunkReaderPassThrough = return

-- | Represents either the end of the PNG, a labelled chunk of metadata or the
-- first chunk of data.
data EndMetadataOrData m r
  = EMDEnd r
  | EMDMetadata ChunkLength ChunkType (ByteString m r)
  | EMDData ChunkLength (ByteString m r)
  deriving (Functor)

instance Monad m => PullsOver m (EndMetadataOrData m) where
  pullOver (EMDEnd r)             = EMDEnd            <$> r
  pullOver (EMDMetadata cl ct bs) = EMDMetadata cl ct <$> pullOver bs
  pullOver (EMDData     cl    bs) = EMDData     cl    <$> pullOver bs

-- | Attempt to read a chunk as metadata. Can successfully return metadata,
-- reach the end of the file, or read a chunk of data.
metadataReader :: (MonadThrow m) => ChunkLength -> ChunkType -> ChunkReader m (EndMetadataOrData m)
metadataReader cl ct
  | ct == "IEND" = fmap (EMDEnd . runIdentity)  . chunkReaderIgnore
  | ct == "IDAT" = fmap (EMDData cl)            . chunkReaderPassThrough
  | otherwise    = fmap (EMDMetadata cl ct)     . chunkReaderPassThrough

data EndOrData m r
  = EODEnd r
  | EODData ChunkLength (ByteString m r)
  deriving (Functor)

instance Monad m => PullsOver m (EndOrData m) where
  pullOver (EODEnd r)      = EODEnd     <$> r
  pullOver (EODData cl bs) = EODData cl <$> pullOver bs

-- | Try to read some data, failing on anything that isn't either data or @IEND@.
-- Returns @'InR' <data>@ if data has been read, or @'InL'@ if we encounter an
-- @IEND@.
--
-- > type MaybeF f r = Sum Identity f r
-- > dataForbidAncillary :: (MonadThrow m) => ChunkType -> ChunkReader (MaybeF (ByteString m)) m
dataReader :: (MonadThrow m) => ChunkLength -> ChunkType -> ChunkReader m (EndOrData m)
dataReader cl ct
  | ct == "IDAT" = fmap (EODData cl)           . chunkReaderPassThrough
  | ct == "IEND" = fmap (EODEnd . runIdentity) . chunkReaderIgnore
  | otherwise    = chunkReaderError (UnexpectedChunk ct)
