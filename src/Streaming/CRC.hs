{-|
Module : Streaming.CRC
Copyright : (c) Bradley Hardy 2016
License: LGPL3
Maintainer: bradleyhardy@live.com
Stability: experimental
Portability: non-portable

-}
module Streaming.CRC (calcCRC32, appendCRC32) where

import           Data.Bits
import           Data.Vector.Unboxed       (Vector)
import qualified Data.Vector.Unboxed       as U
import           Data.Word                 (Word32, Word8)
import qualified Data.Serialize                    as C

import qualified Data.ByteString           as B
import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as Q
import           Streaming.Prelude         (Of (..))

crc32Table :: Vector Word32
crc32Table = U.generate 256 (calc . fromIntegral)
  where
    calc :: Word8 -> Word32
    calc = go 8 . fromIntegral
      where
        go :: Int -> Word32 -> Word32
        go 0 c = c
        go k c =
          if c .&. 1 > 0
          then go (k - 1) (0xedb88320 `xor` shiftR c 1)
          else go (k - 1) (shift c (-1))

updateCRC32 :: Word32 -> B.ByteString -> Word32
updateCRC32 crc bytes
  | Just (h, tl) <- B.uncons bytes =
    let index = fromIntegral ((crc `xor` fromIntegral h) .&. 0xff)
    in updateCRC32 ((crc32Table U.! index) `xor` shiftR crc 8) tl
  | otherwise = crc

-- | Calculate the CRC of a streaming 'ByteString', consuming the input and
-- returning the CRC paired with the 'ByteString''s return value.
calcCRC32 :: Monad m => ByteString m r -> m (Of Word32 r)
calcCRC32 input =
  do (res :> x) <- Q.foldlChunks updateCRC32 0xffffffff input
     return (res `xor` 0xffffffff :> x)

-- | If the input stream is finite, calculate its CRC and append it to the end
-- (maintaining streaming). The is the identity function for infinite inputs.
appendCRC32 :: Monad m => ByteString m r -> ByteString m r
appendCRC32 input =
  do (crc :> res) <- calcCRC32 $ Q.copy input
     Q.fromStrict (C.runPut . C.putWord32be $ crc)
     return res
