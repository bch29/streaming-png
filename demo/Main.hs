{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Codec.Picture                     (DynamicImage)
import qualified Codec.Picture.Png                 as Juicy
import           Codec.Picture.Png.Streaming
import           Codec.Picture.Png.Streaming.Juicy

import           Control.Monad                     (void)
import           Control.Monad.IO.Class            (MonadIO (..))
import           Control.Monad.Trans.Resource      (MonadResource)

import qualified Data.ByteString                   as B

import           Criterion.Main

import qualified Data.ByteString.Streaming         as Q
import           Streaming.Prelude                 (Of (..))

readPNG :: (MonadResource m) => FilePath -> m DynamicImage
readPNG readPath =
  do decoded <- decodePNGFile readPath
     res :> _ <- imageFromStream decoded
     return res

decodeBS :: B.ByteString -> IO DynamicImage
decodeBS bs =
  do decoded <- decodePNG $ Q.fromStrict bs
     res :> _ <- imageFromStream decoded
     return res

copyPNG :: (MonadResource m) => FilePath -> FilePath -> m ()
copyPNG readPath writePath =
  do image <- readPNG readPath
     liftIO . void $ Juicy.writeDynamicPng writePath image

fstOf :: Of a b -> a
fstOf (x :> _) = x

benchImage :: (String, B.ByteString) -> [Benchmark]
benchImage (path, image) =
  [ bench ("read " ++ path ++ " with JuicyPixels") . nf Juicy.decodePng $ image
  , bench ("read " ++ path ++ " with streaming") . nfIO $ decodeBS image
  ]

benchmarkMain :: IO ()
benchmarkMain =
  do largeFile <- B.readFile "large.png"
     mediumFile <- B.readFile "medium.png"
     smallFile <- B.readFile "small.png"

     defaultMain (benchImage =<< [ ("small.png", smallFile)
                                 , ("medium.png", mediumFile)
                                 , ("large.png", largeFile)])

main :: IO ()
-- main = runResourceT $ copyPNG "large.png" "result.png"
main = benchmarkMain
