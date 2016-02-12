{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Codec.Picture                           (DynamicImage)
import           Codec.Picture.Png                       (writeDynamicPng)
import           Codec.Picture.Png.Streaming
import           Codec.Picture.Png.Streaming.JuicyPixels

import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (MonadIO (..))
import           Control.Monad.Trans.Resource            (MonadResource,
                                                          runResourceT)

import           Streaming.Prelude                       (Of (..))

readPNG :: (MonadResource m) => FilePath -> m DynamicImage
readPNG readPath =
  do decoded <- decodePNGFile readPath
     res :> _ <- imageFromStream decoded
     return res

copyPNG :: (MonadResource m) => FilePath -> FilePath -> m ()
copyPNG readPath writePath =
  do image <- readPNG readPath
     liftIO . void $ writeDynamicPng writePath image

-- benchmarkMain :: IO ()
-- benchmarkMain =
--   defaultMain
--   [ bench "readPNG" $ nfIO (runResourceT $ readPNG "test.png")
--   ]

main :: IO ()
main = runResourceT $ copyPNG "test.png" "result.png"
