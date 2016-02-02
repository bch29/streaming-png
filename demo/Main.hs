{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Codec.Picture.Png.Streaming

import           Control.Monad.Catch          (MonadThrow)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.ByteString.Streaming    (ByteString)
import qualified Data.ByteString.Streaming    as Q
import qualified Streaming                    as S
import           Streaming.Prelude            (Of (..))

copyPNGDataToFiles :: (MonadResource m) => FilePath -> FilePath -> ByteString m r -> m (HeaderData, r)
copyPNGDataToFiles metaFp dataFp input =
  let raw = decodePNGComplete input
  in do (hdr :> metadata) <- raw
        mainData <- Q.writeFile metaFp $ Q.concat $ S.maps unlabelByteString metadata
        res      <- Q.writeFile dataFp $ Q.concat $ S.maps unlabelByteString mainData
        return (hdr, res)

test :: IO HeaderData
test = runResourceT $ fmap fst $ copyPNGDataToFiles "metadata.dat" "result.dat" $ Q.readFile "reggie.png"

copyPNG :: (MonadResource m) => FilePath -> FilePath -> m ()
copyPNG readPath writePath = Q.writeFile writePath $ encodePNG $ decodePNGComplete $ Q.readFile readPath

test2 :: IO ()
test2 = runResourceT $ copyPNG "reggie.png" "result.png"

main :: IO ()
main = print =<< test2
