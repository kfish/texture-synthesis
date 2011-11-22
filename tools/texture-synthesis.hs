{-# OPTIONS -Wall #-}

module Main where

import Graphics.TextureSynthesis

main :: IO ()
main = do
    let ts = mkTexture 8
    print ts
