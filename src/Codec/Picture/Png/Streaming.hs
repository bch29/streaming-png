{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
module Codec.Picture.Png.Streaming
       (
         -- * Types
         PNGDecodeError
       , DecodedPNG
       , HeaderData(..)

         -- * Decoding
       , decodePNG
       , decodePNGComplete
       , decodePNGFile

         -- * Misc
       , BitDepth
       , ColourType
       , CompressionMethod
       , FilterMethod
       , InterlaceMethod

       , isColourTypeSupported
       , isCompressionMethodSupported
       , isFilterMethodSupported
       , isInterlaceMethodSupported
       )
       where

import           Codec.Picture.Png.Streaming.Core
import           Codec.Picture.Png.Streaming.Header
import           Codec.Picture.Png.Streaming.Info
import           Codec.Picture.Png.Streaming.MainData
import           Codec.Picture.Png.Streaming.Util

import           Control.Monad                        (ap, join, unless, when)
import           Control.Monad.Catch                  (MonadThrow (..))
import           Control.Monad.IO.Class               (MonadIO(..))
import           Control.Monad.Morph                  (hoist)
import           Control.Monad.Trans                  (MonadTrans (..))
import           Control.Monad.Trans.Resource         (MonadResource)
import           Data.Functor.Identity                (Identity (..))
import           Data.Functor.Sum                     (Sum (..))

import qualified Data.ByteString                      as B
import           Data.ByteString.Streaming            (ByteString)
import qualified Data.ByteString.Streaming            as Q
import qualified Streaming                            as S
import           Streaming.Prelude                    (Of (..), Stream)

type DecodedPNG m r = Of HeaderData (ByteString m r)

{-|
Decode a PNG from the given raw streaming 'ByteString'. The result is header
information followed by a stream of bytes that can be interpreted directly as
pixels whose format depends on the image's colour type.

Any remaining data after the end of the PNG image is returned untouched.
-}
decodePNG :: (MonadThrow m, MonadIO m) => ByteString m r -> m (DecodedPNG m (ByteString m r))
decodePNG input =
  do (hd :> rest) <- takeHeaderData (decodePNGChunks input)
     unless (isImageTypeSupported hd) (throwM UnsupportedImageType)
     return (hd :> decodeImageData hd rest)

{-|
Decode a PNG from the given raw streaming 'ByteString'. The result is header
information followed by a stream of bytes that can be interpreted directly as
pixels whose format depends on the image's colour type.

If there is any more data after the end of the PNG image, an 'ExpectedEOF'
exception is thrown.
-}
decodePNGComplete :: (MonadThrow m, MonadIO m) => ByteString m r -> m (DecodedPNG m r)
decodePNGComplete input =
  do (hd :> rest) <- decodePNG input
     let rest' = lift . expectNull ExpectedEOF =<< rest
     return (hd :> rest')

{-|
Decode a PNG from the given file. The result is header information followed by a
stream of bytes that can be interpreted directly as pixels whose format depends
on the image's colour type.

If there is any more data after the end of the PNG image, an 'ExpectedEOF'
exception is thrown.
-}
decodePNGFile :: (MonadResource m) => FilePath -> m (DecodedPNG m ())
decodePNGFile = decodePNGComplete . Q.readFile

-- | Is the PNG image type described by the given header supported?
isImageTypeSupported :: HeaderData -> Bool
isImageTypeSupported HeaderData{..} =
  isColourTypeSupported hdColourType &&
  isCompressionMethodSupported hdCompressionMethod &&
  isFilterMethodSupported hdFilterMethod &&
  isInterlaceMethodSupported hdInterlaceMethod

-- | Is the given PNG image colour type supported? Currently we only support
-- non-indexed colour types.
isColourTypeSupported :: ColourType -> Bool
isColourTypeSupported = (`elem` [0, 2, 4, 6])

-- | Is the given PNG compression method supported? Currently we only support
-- method 0.
isCompressionMethodSupported :: CompressionMethod -> Bool
isCompressionMethodSupported = (== 0)

-- | Is the given PNG filter method supported? Currently we only support method
-- 0.
isFilterMethodSupported :: FilterMethod -> Bool
isFilterMethodSupported = (== 0)

-- | Is the given PNG interlace method supported? Currently no interlace methods
-- are supported.
isInterlaceMethodSupported :: InterlaceMethod -> Bool
isInterlaceMethodSupported = (== 0)
