{-# OPTIONS -Wall #-}

module Main where

import Graphics.TextureSynthesis

main :: IO ()
main = do
    ts <- mkTexture 8
    print ts
