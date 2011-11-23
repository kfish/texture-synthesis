{-# OPTIONS -Wall #-}

module Main (main) where

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Graphics.TextureSynthesis

----------------------------------------------------------------------

-- | Check that a tile of depth n is fully filled.
-- The size of tiles follows sequence A028400 (2^n + 1)^2
fillTile :: Int -> Bool
fillTile n = length (flattenTexture (n+1) (genTextureDefault n)) == square (2^n +1)
    where
        square x = x * x

----------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "fillTile"
      [ testProperty "0" (fillTile 0)
      , testProperty "1" (fillTile 1)
      , testProperty "2" (fillTile 2)
      , testProperty "3" (fillTile 3)
      , testProperty "4" (fillTile 4)
      , testProperty "5" (fillTile 5)
      , testProperty "6" (fillTile 6)
      , testProperty "7" (fillTile 7)
      , testProperty "8" (fillTile 8)
      , testProperty "9" (fillTile 9)
      , testProperty "10" (fillTile 10)
      ]
    ]
