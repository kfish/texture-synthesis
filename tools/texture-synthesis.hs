{-# OPTIONS -Wall #-}

module Main where

import Graphics.TextureSynthesis

main :: IO ()
main = do
    ts <- genTexture 8
    print ts
