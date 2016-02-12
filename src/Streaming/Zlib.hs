{-# LANGUAGE OverloadedStrings #-}
module Streaming.Zlib
       (
         -- * Decompressing and compressing streams
         decompressStream
       , decompressStream'
       , compressStream
       , compressStream'

         -- * Re-exported from 'Codec.Compression.Zlib'
       , CompressionLevel
       , WindowBits
       , defaultWindowBits
       , defaultCompressionLevel)
       where

import Codec.Picture.Png.Streaming.Util

import           Data.Streaming.Zlib                (WindowBits, defaultWindowBits, PopperRes(PRDone, PRNext, PRError), Popper)
import qualified Data.Streaming.Zlib                as Z

import           Control.Monad.IO.Class    (MonadIO (..))
import           Control.Monad.Catch       (MonadThrow (..))

import qualified Data.ByteString           as B
import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as Q

exhaustPopper
  :: (MonadIO m, MonadThrow m)
     => Popper
     -> ByteString m ()
exhaustPopper popper = go
  where
    go = do pres <- liftIO popper
            case pres of
              PRDone -> return ()
              PRNext bs -> do Q.chunk bs; go
              PRError e -> throwM e

-- | Use Zlib to decompress a 'ByteString' with the specified 'WindowBits'.
decompressStream'
  :: (MonadIO m, MonadThrow m)
     => WindowBits
     -> ByteString m r
     -> ByteString m r
decompressStream' windowBits input =
  do inflate <- liftIO $ Z.initInflate windowBits

     let build bs =
           do mchnk <- Q.nextChunk bs
              case mchnk of
                Left r -> return (Left r)
                Right (chnk, bs') ->
                  do popper <- liftIO $ Z.feedInflate inflate chnk
                     return (Right (exhaustPopper popper, bs'))

     res <- buildByteString build input

     remaining <- liftIO $ Z.finishInflate inflate
     Q.chunk remaining

     return res

-- | Use Zlib to decompress a 'ByteString' with the default parameters.
decompressStream
  :: (MonadIO m, MonadThrow m)
     => ByteString m r
     -> ByteString m r
decompressStream = decompressStream' defaultWindowBits

type CompressionLevel = Int

-- | The default compression level is 6.
defaultCompressionLevel :: CompressionLevel
defaultCompressionLevel = 6

-- | Use Zlib to compress a 'ByteString' with the specified 'CompressionLevel'
-- and 'WindowBits'.
compressStream'
  :: (MonadIO m, MonadThrow m)
     => CompressionLevel
     -> WindowBits
     -> ByteString m r
     -> ByteString m r
compressStream' compressLevel windowBits input =
  do deflate <- liftIO $ Z.initDeflate compressLevel windowBits

     let build bs =
           do mchnk <- Q.nextChunk bs
              case mchnk of
                Left r -> return (Left r)
                Right (chnk, bs') ->
                  do popper <- liftIO $ Z.feedDeflate deflate chnk
                     return (Right (exhaustPopper popper, bs'))

     res <- buildByteString build input

     exhaustPopper $ Z.finishDeflate deflate

     return res

-- | Use Zlib to compress a 'ByteString' with the default parameters.
compressStream
  :: (MonadIO m, MonadThrow m)
     => ByteString m r
     -> ByteString m r
compressStream = compressStream' defaultCompressionLevel defaultWindowBits

testStr :: ByteString IO ()
testStr = "hello, my name is Brad"

test :: IO B.ByteString
test = Q.toStrict_ $ decompressStream $ compressStream testStr
