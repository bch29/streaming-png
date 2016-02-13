{-|
Module : Codec.Picture.Png.Streaming.Util
Copyright : (c) Bradley Hardy 2016
License: LGPL3
Maintainer: bradleyhardy@live.com
Stability: experimental
Portability: portable

-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codec.Picture.Png.Streaming.Util where

import           Control.Monad.Catch       (Exception, MonadThrow (..))
import           Control.Monad.Morph       (generalize)
import           Control.Monad.Trans       (MonadTrans (..))
import qualified Data.ByteString.Internal  as BI
import           Data.Functor.Identity     (Identity (..))
import           Data.Functor.Sum          (Sum (..))
import           Data.Int                  (Int64)
import           Data.Vector.Storable      (Vector)
import qualified Data.Vector.Storable      as Vec
import           Data.Word                 (Word8)

import           Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming as Q
import           Streaming                 (Stream)
import qualified Streaming                 as S
import           Streaming.Prelude         (Of (..))
import qualified Streaming.Prelude         as S

-- | If the input 'ByteString' is empty, return its result. Otherwise throw the
-- provided error value.
expectNull :: (MonadThrow m, Exception e) => e -> ByteString m r -> m r
expectNull err input =
  do headChunk <- Q.nextChunk input
     case headChunk of
       Left res -> return res
       Right _ -> throwM err

-- | Split a streaming ByteString up into a stream of chunks of the given size.
chunksOfBS :: (Monad m) => Int64 -> ByteString m r -> Stream (ByteString m) m r
chunksOfBS n input =
  do isEmpty <- lift (Q.null_ input)
     if isEmpty
       then lift (Q.effects input)
       else do rest <- S.yields (Q.splitAt n input)
               chunksOfBS n rest

-- | Remember the previous value at each point in a stream of values.
rememberPrevious :: (Monad m) => Stream (Of a) m r -> Stream (Of (Maybe a, a)) m r
rememberPrevious = go Nothing
  where
    go prevItem input =
      let continue (thisItem, rest) =
            do S.yields ((prevItem, thisItem) :> ())
               go (Just thisItem) rest
      in lift (S.next input) >>= either return continue

-- | Map a function across a stream, but also include in its arguments the
-- result of applying it to the previous item in the stream (which is 'Nothing'
-- if the current item is the first in the stream).
mapWithMemory :: forall m a b r. (Monad m) => (Maybe b -> a -> m b) -> Stream (Of a) m r -> Stream (Of b) m r
mapWithMemory f input = S.unfoldr step (Nothing, input)
  where
    step :: (Maybe b, Stream (Of a) m r) -> m (Either r (b, (Maybe b, Stream (Of a) m r)))
    step (prevResult, stream) =
      let handleItem (thisItem, remainder) =
             do thisResult <- f prevResult thisItem
                return (Right (thisResult, (Just thisResult, remainder)))
      in S.next stream >>= either (return . Left) handleItem

{-|

For each functor wrapper @f@ in the stream, either strip it off or convert it to
some new functor @g@. Return the stream of @g@'s. This can be seen to be
analogous to a list function of type @'Monad' m => (a -> m ('Maybe' b)) -> [a]
-> m [b]@ if we consider what it looks like when @f@ and @g@ are @'Of' a@ and
@'Of' b@ respectively:

> filterMapped
>   :: (forall x. Of a x -> m (Sum Identity (Of b) x))
>   -> Stream (Of a) m r -> Stream (Of b) m r

Here, @'Sum' 'Identity' ('Of' b) x@ is isomorphic to @'Of' ('Maybe' b) x@.

-}

filterMapped
  :: (Monad m, Functor f, Functor g)
     => (forall x. f x -> m (Sum Identity g x))
     -> Stream f m r
     -> Stream g m r
filterMapped f = S.run . S.maps generalize . S.separate . S.mapped f

{-|

For each functor wrapper in the stream, optionally strip it off. Those stripped
off will be removed from the resulting stream.

-}

filtered
  :: (Monad m, Functor f)
     => (forall x. f x -> m (Maybe x))
     -> Stream f m r
     -> Stream f m r
filtered f =
  let f' x = maybe (InR x) (InL . Identity) <$> f x
  in filterMapped f'


-- | Build a 'ByteString' monadically from a seed.
buildByteString
  :: Monad m
     => (a -> m (Either r (ByteString m (), a)))
     -> a
     -> ByteString m r
buildByteString build seed =
  do mx <- lift $ build seed
     case mx of
       Left r -> return r
       Right (bs, seed') ->
         do bs
            buildByteString build seed'

-- | Directly convert a 'BI.ByteString' into a storable 'Vector', in constant
-- time.
bytestringToVector :: BI.ByteString -> Vector Word8
bytestringToVector (BI.PS fptr offset idx) = Vec.unsafeFromForeignPtr fptr offset idx

-- | Directly convert a storable 'Vector' of 'Word8's into a 'BI.ByteString', in
-- constant time.
vectorToBytestring :: Vector Word8 -> BI.ByteString
vectorToBytestring v
  | (fptr, offset, idx) <- Vec.unsafeToForeignPtr v =
    BI.PS fptr offset idx
