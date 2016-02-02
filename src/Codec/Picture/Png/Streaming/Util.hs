module Codec.Picture.Png.Streaming.Util where

import           Control.Monad.Catch       (Exception, MonadThrow (..))

import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as Q

-- | If the input 'ByteString' is empty, return its result. Otherwise throw the
-- provided error value.
expectNull :: (MonadThrow m, Exception e) => e -> ByteString m r -> m r
expectNull err input =
  do headChunk <- Q.nextChunk input
     case headChunk of
       Left res -> return res
       Right _ -> throwM err
