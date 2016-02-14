{-|
Module : Codec.Picture.Png.Streaming.Metadata
Copyright : (c) Bradley Hardy 2016
License: LGPL3
Maintainer: bradleyhardy@live.com
Stability: experimental
Portability: portable

-}

{-# LANGUAGE DeriveFunctor #-}
module Codec.Picture.Png.Streaming.Metadata where

import           Codec.Picture.Png.Streaming.Core
import           Codec.Picture.Png.Streaming.Header
import           Codec.Picture.Png.Streaming.Info
import           Codec.Picture.Png.Streaming.Util
import           Streaming.Zlib

import           Control.Monad.Catch                (MonadThrow (..))
import           Control.Monad.IO.Class             (MonadIO (..))

import qualified Data.ByteString                    as B
import qualified Data.ByteString.Unsafe             as B
import           Data.Functor.Identity              (Identity (..))
import           Data.Functor.Sum                   (Sum (..))
import           Data.Int                           (Int64)
import           Data.Text                          (Text)
import qualified Data.Vector.Storable               as Vec
import qualified Data.Vector.Storable.Mutable       as Vec
import           Data.Word                          (Word16, Word32, Word64,
                                                     Word8)

import           Data.ByteString.Streaming          (ByteString)
import qualified Data.ByteString.Streaming          as Q
import           Streaming                          (Of (..), Stream)
import qualified Streaming                          as S

data SizedByteString m r =
  SizedByteString
  { bsLength :: Word32
  , bsData :: ByteString m r
  }
  deriving (Functor)

data MetadataChunk m r
  = MCChromaticities         Chromaticities         r
  | MCGamma                  Gamma                  r
  | MCSignificantBits        SignificantBits        r
  | MCStandardRgbColourSpace StandardRgbColourSpace r
  | MCBackgroundColour       BackgroundColour       r
  | MCPixelDimensions        PixelDimensions        r
  | MCModificationTime       ModificationTime       r
  | MCIccProfile             IccProfile           (SizedByteString m r)
  | MCPngText                PngText              (SizedByteString m r)
  | MCPngInternationalText   PngInternationalText (SizedByteString m r)
  | MCHistogram                                   (SizedByteString m r)
  | MCSuggestedPalette       SuggestedPalette     (SizedByteString m r)
  | MCTransparency           (Transparency m r)
  | MCUnknownChunk           (PNGChunk m r)
  deriving (Functor)

data Transparency m r
  = Tr0 Word16 r
  | Tr2 Word16 Word16 Word16 r
  | Tr3 (SizedByteString m r)
  deriving (Functor)

data Chromaticities =
  Chromaticities
  { chWhitePointX :: Word32
  , chWhitePointY :: Word32
  , chRedX        :: Word32
  , chRedY        :: Word32
  , chGreenX      :: Word32
  , chGreenY      :: Word32
  , chBlueX       :: Word32
  , chBlueY       :: Word32
  }

data Gamma =
  Gamma
  { gaImageGamma :: Word32
  }

data IccProfile =
  IccProfile
  { icProfileName       :: Text
  , icCompressionMethod :: Word8
  }

data SignificantBits
  = SB0 Word8 -- ^ Significant bits information for colour type 0
  | SB23 Word8 Word8 Word8 -- ^ Significant bits information for colour types 2 or 3
  | SB4 Word8 Word8 -- ^ Significant bits information for colour type 4
  | SB6 Word8 Word8 Word8 Word8 -- ^ Significant bits information for colour type 6

data StandardRgbColourSpace =
  StandardRgbColourSpace
  { csRenderingIntent :: Word8
  }

data PngText =
  PngText
  { txKeyword  :: Text
  }

data PngInternationalText =
  PngInternationText
  { itKeyword           :: Text
  , itLanguageTag       :: Text
  , itTranslatedKeyword :: Text
  }

data BackgroundColour
  = BC04 Word16 -- ^ Background colour information for colour types 0 or 4
  | BC26 Word16 Word16 Word16 -- ^ Background colour information for colour types 2 or 6
  | BC3 Word8 -- ^ Background colour information for colour type 3

data PixelDimensions =
  PixelDimensions
  { pdPpuX :: Word32 -- ^ Pixels per unit on the X axis
  , pdPpuY :: Word32 -- ^ Pixels per unit on the Y axis
  , pdUnit :: Word8
  }

data SuggestedPalette =
  SuggestedPalette
  { spName        :: Text
  , spSampleDepth :: Word8
  }

data ModificationTime =
  ModificationTime
  { mtYear   :: Word16
  , mtMonth  :: Word8
  , mtDay    :: Word8
  , mtHour   :: Word8
  , mtMinute :: Word8
  , mtSecond :: Word8
  }
