{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main where

import Control.Applicative ((<$>))
import Data.ZoomCache
import Data.ZoomCache.Texture
import Graphics.TextureSynthesis

main :: IO ()
main = do
    ss <- slices <$> flattenTexture 6 <$> genTexture 5
    -- ss <- slices <$> flattenTexture 5 <$> genTexture 4
    -- ss <- slices <$> flattenTexture 3 <$> genTexture 2

    let ts = map TextureSlice ss
        track = oneTrack (undefined :: TextureSlice) False False ConstantDR 1000 "texture"

    withFileWrite track True (mapM_ (write 1) ts) "texture.zoom"
