{-# LANGUAGE DeriveDataTypeable #-}
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
) where

import Control.Applicative ((<$>))
import Control.Monad (replicateM)
import Data.ByteString (ByteString)
import Data.Iteratee (Iteratee)
import Data.Typeable
import Data.ZoomCache.Codec

----------------------------------------------------------------------

textureLength :: Int
textureLength = 8

----------------------------------------------------------------------

data TextureSlice = TextureSlice [Float]
    deriving (Show, Typeable)

----------------------------------------------------------------------

-- Identifier for track headers
trackTypeTexture :: ByteString
trackTypeTexture = "ZTEXf32b"

----------------------------------------------------------------------
-- Read

instance ZoomReadable TextureSlice where
    data SummaryData TextureSlice = SummaryTextureSlice
        { summaryAvgs :: [Float]
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
