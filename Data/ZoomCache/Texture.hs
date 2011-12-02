{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Data.ZoomCache.Texture
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

Zoom-cache codec implementation for textures, defined here as timeslices
of type [Float].

The table below describes the encoding of SummaryData for TextureSlice:

@
   | ...                                                           |   -35
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Row 1 mean (float)                                            | 36-39
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Row 2 mean (float)                                            | 40-43
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Row 3 mean (float)                                            | 44-47
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | Row 4 mean (float)                                            | 48-51
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   | ...                                                           | 52-
@

Field encoding formats:

  @float@:  big-endian IEEE 754-2008 binary32 (IEEE 754-1985 single)

-}
----------------------------------------------------------------------

module Data.ZoomCache.Texture (
    -- * Types
      TextureSlice(..)

    -- * Iteratee reading of textures
    , enumTexture
    , enumSummaryTexture

    -- * Zoom-cache codec instances
    , SummaryData(..)
    , SummaryWork(..)

    -- * Codec identifiers
    , textureIdentifiers
) where

import Blaze.ByteString.Builder
import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Control.Monad.Trans (MonadIO)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import qualified Data.Iteratee as I
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (mconcat)
import Data.Typeable
import Data.ZoomCache
import Data.ZoomCache.Codec

----------------------------------------------------------------------

textureLength :: Int
textureLength = (2^5)+1
-- textureLength = 17
-- textureLength = 5

----------------------------------------------------------------------

data TextureSlice = TextureSlice [Float]
    deriving (Show, Typeable)

textureIdentifiers :: [IdentifyCodec]
textureIdentifiers =
    [ identifyCodec (undefined :: TextureSlice)
    ]

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeTexture :: ByteString
trackTypeTexture = "ZTEXf32b"

----------------------------------------------------------------------
-- Read

instance ZoomReadable TextureSlice where
    data SummaryData TextureSlice = SummaryTextureSlice
        { summaryAvgs :: ![Float]
        }

    trackIdentifier = const trackTypeTexture

    readRaw     = TextureSlice <$> readSlice
    readSummary = SummaryTextureSlice <$> readSlice

    prettyRaw         = prettyPacketTexture
    prettySummaryData = prettySummaryTexture

readSlice :: (Functor m, Monad m) => Iteratee ByteString m [Float]
readSlice = replicateM textureLength readFloat32be

prettyPacketTexture :: TextureSlice -> String
prettyPacketTexture = show

prettySummaryTexture :: SummaryData TextureSlice -> String
prettySummaryTexture SummaryTextureSlice{..} = show summaryAvgs

----------------------------------------------------------------------
-- Write

instance ZoomWrite TextureSlice where
    write = writeData

instance ZoomWrite (SampleOffset, TextureSlice) where
    write = writeDataVBR

instance ZoomWritable TextureSlice where
    data SummaryWork TextureSlice = SummaryWorkTextureSlice
        { swTextureTime :: {-# UNPACK #-}!SampleOffset
        , swTextureSums :: ![Float]
        }
    fromRaw           = fromTexture
    fromSummaryData   = fromSummaryTexture

    initSummaryWork   = initSummaryTexture
    toSummaryData     = mkSummaryTexture
    updateSummaryData = updateSummaryTexture
    appendSummaryData = appendSummaryTexture

fromSlice :: [Float] -> Builder
fromSlice = mconcat . map fromFloat

fromTexture :: TextureSlice -> Builder
fromTexture (TextureSlice ts) = fromSlice ts

fromSummaryTexture :: SummaryData TextureSlice -> Builder
fromSummaryTexture (SummaryTextureSlice ts) = fromSlice ts

initSummaryTexture :: SampleOffset -> SummaryWork TextureSlice
initSummaryTexture entry = SummaryWorkTextureSlice {
      swTextureTime = entry
    , swTextureSums = replicate textureLength 0
    }

mkSummaryTexture :: SampleOffsetDiff -> SummaryWork TextureSlice
                 -> SummaryData TextureSlice
mkSummaryTexture (SODiff dur) sw = SummaryTextureSlice {
        summaryAvgs = map (/ dur') (swTextureSums sw)
    }
    where
        dur' = fromIntegral dur

updateSummaryTexture :: SampleOffset -> TextureSlice
                     -> SummaryWork TextureSlice
                     -> SummaryWork TextureSlice
updateSummaryTexture t (TextureSlice ds) sw = SummaryWorkTextureSlice {
      swTextureTime = t
    , swTextureSums = zipWith (+) ds' (swTextureSums sw)
    }
    where
        ds' = map (realToFrac . (* fromIntegral dur)) ds
        !(SODiff dur) = sampleOffsetDiff t (swTextureTime sw)

appendSummaryTexture :: SampleOffsetDiff -> SummaryData TextureSlice
                     -> SampleOffsetDiff -> SummaryData TextureSlice
                     -> SummaryData TextureSlice
appendSummaryTexture (SODiff dur1) s1 (SODiff dur2) s2 = SummaryTextureSlice {
        summaryAvgs = zipWith avg (summaryAvgs s1) (summaryAvgs s2)
    }
    where
        avg d1 d2 = (d1 * dur1') + (d2 * dur2') / durSum
        dur1' = fromIntegral dur1
        dur2' = fromIntegral dur2
        durSum = fromIntegral (dur1 + dur2)

----------------------------------------------------------------------

rawToTexture :: ZoomRaw -> [TextureSlice]
rawToTexture (ZoomRaw xs) | typeOf xs == typeOf (undefined :: [TextureSlice]) =
                               fromMaybe [] (cast xs :: Maybe [TextureSlice])
                          | otherwise = []

enumTexture :: (Functor m, MonadIO m)
            => I.Enumeratee [Stream] [(SampleOffset, TextureSlice)] m a
enumTexture = I.joinI . enumPacketSOs . I.mapChunks (concatMap f)
    where
        f :: PacketSO -> [(SampleOffset, TextureSlice)]
        f PacketSO{..} = zip packetSOSampleOffsets (rawToTexture packetSOData)

enumSummaryTexture :: (Functor m, MonadIO m)
                   => Int
                   -> I.Enumeratee [Stream] [SummarySO TextureSlice] m a
enumSummaryTexture level =
    I.joinI . enumSummarySOLevel level .
    I.mapChunks (catMaybes . map toSD)
    where
        toSD :: ZoomSummarySO -> Maybe (SummarySO TextureSlice)
        toSD (ZoomSummarySO s) = cast s
