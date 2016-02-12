{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}
module Codec.Picture.Png.Streaming.JuicyPixels
       ( imageFromStream )
       where

import           Codec.Picture
import           Codec.Picture.Png.Streaming
import           Codec.Picture.Png.Streaming.Core

import           Control.Monad.Catch              (MonadThrow (..))

import qualified Data.ByteString.Internal         as BI
import           Data.Vector.Storable             (Vector)
import qualified Data.Vector.Storable             as Vec
import           Data.Word                        (Word16, Word8)
import           Foreign                          (castForeignPtr, sizeOf)

import           Data.ByteString.Streaming        (ByteString)
import qualified Data.ByteString.Streaming        as Q
import           Streaming.Prelude                (Of (..))

type BytePacker m b r = ByteString m r -> m (Of (Vector b) r)
type MkImage m p r = (Int, Int) -> ByteString m r -> m (Of (Image p) r)

bp8 :: (Monad m) => BytePacker m Word8 r
bp8 input =
  do BI.PS fptr offset idx :> res <- Q.toStrict input
     return (Vec.unsafeFromForeignPtr fptr offset idx :> res)

bp16 :: (Monad m) => BytePacker m Word16 r
bp16 input =
  do BI.PS fptr offset idx :> res <- Q.toStrict input
     let ws = sizeOf (0 :: Word16)
     return (Vec.unsafeFromForeignPtr (castForeignPtr fptr) (offset `div` ws) (idx `div` ws) :> res)

mkImage :: (Monad m) => BytePacker m (PixelBaseComponent p) r -> MkImage m p r
mkImage bytePacker (imageWidth, imageHeight) input =
  do imageData :> res <- bytePacker input
     return (Image{..} :> res)

lmap :: (a -> b) -> Of a r -> Of b r
lmap f (x :> r) = f x :> r

-- | Pulls a PNG image stream into memory as a JuicyPixels 'DynamicImage'.
-- Currently supports every legal non-indexed colour type.
imageFromStream :: (MonadThrow m) => DecodedPNG m r -> m (Of DynamicImage r)
imageFromStream (hd@HeaderData{..} :> bytes) =
  let wh = (fromIntegral hdWidth, fromIntegral hdHeight)
  in if | hdColourType == 0 && hdBitDepth == 8  -> lmap ImageY8     <$> mkImage bp8  wh bytes
        | hdColourType == 0 && hdBitDepth == 16 -> lmap ImageY16    <$> mkImage bp16 wh bytes
        | hdColourType == 2 && hdBitDepth == 8  -> lmap ImageRGB8   <$> mkImage bp8  wh bytes
        | hdColourType == 2 && hdBitDepth == 16 -> lmap ImageRGB16  <$> mkImage bp16 wh bytes
        | hdColourType == 4 && hdBitDepth == 8  -> lmap ImageYA8    <$> mkImage bp8  wh bytes
        | hdColourType == 4 && hdBitDepth == 16 -> lmap ImageYA16   <$> mkImage bp16 wh bytes
        | hdColourType == 6 && hdBitDepth == 8  -> lmap ImageRGBA8  <$> mkImage bp8  wh bytes
        | hdColourType == 6 && hdBitDepth == 16 -> lmap ImageRGBA16 <$> mkImage bp16 wh bytes
        | otherwise -> throwM UnsupportedImageType
