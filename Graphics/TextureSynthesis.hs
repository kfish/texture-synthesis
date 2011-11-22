{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Graphics.TextureSynthesis (
      Texture(..)
    , textureEmpty
    , mkTexture
    , flattenTexture
) where

import Control.Parallel
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.Random.MWC as MWC

-- | Point on the 2D plane
data I2 = I2 !Int !Int
    deriving (Eq, Ord, Show)

----------------------------------------------------------------------

data QuadTree a = QuadTree {
      treeLevel      :: {-# UNPACK #-}!Int
    , c              :: !a
    , n, e, w, s     :: !a
    , nw, ne, sw, se :: !(QuadTree a)
    } | QuadNil
    deriving (Show)

data Texture a = Texture {
      topLeft, topRight, botLeft, botRight :: !a
    , tree :: !(QuadTree a)
    }
    deriving (Show)

textureEmpty :: Texture Float
textureEmpty = Texture 0 0 0 0 QuadNil

mkTexture :: Int -> IO (Texture Float)
mkTexture !lim = do
    quad <- MWC.withSystemRandom (mkQuad lim 0 0 0 0 0)
    return (Texture 0 0 0 0 quad)

mkQuad :: (Fractional a)
       => Int -> Int -> a -> a -> a -> a
       -> MWC.GenIO -> IO (QuadTree a)
mkQuad !lim !lvl !tL !tR !bL !bR gen
    | lvl >= lim = return QuadNil
    | otherwise  = do
        let !lvl' = lvl+1
            !c' = (tL + tR + bL + bR) / 4
            !n' = (tL + tR) / 2
            !e' = (tR + bR) /2
            !w' = (tL + bL) /2
            !s' = (bL + bR) /2
        nw' <- mkQuad lim lvl' tL n' w' c' gen
        ne' <- mkQuad lim lvl' n' tR c' e' gen
        sw' <- mkQuad lim lvl' w' c' bL s' gen
        se' <- mkQuad lim lvl' c' e' s' bR gen
        return QuadTree
            { treeLevel = lvl'
            , c = c'
            , n = n'
            , e = e'
            , w = w'
            , s = s'
            , nw = nw'
            , ne = ne'
            , sw = sw'
            , se = se'
            }

flattenTexture :: Int -> Texture a -> [(I2, a)]
flattenTexture !n Texture{..} =
    [ (I2 0 0, topLeft)
    , (I2 l 0, topRight)
    , (I2 0 l, botLeft)
    , (I2 l l, botRight)
    ] ++ flattenQuad n (I2 0 0) (I2 l l) tree
    where
        l = 2^n

flattenQuad :: Int -> I2 -> I2 -> QuadTree a -> [(I2, a)]
flattenQuad !lim !x1y1 !x2y2 !q = Map.assocs (quadToMap lim x1y1 x2y2 q)
   
quadToMap :: Int -> I2 -> I2 -> QuadTree a -> Map I2 a
quadToMap _   _          _          QuadNil      = Map.empty
quadToMap !lim !(I2 x1 y1) !(I2 x2 y2) QuadTree{..}
    | treeLevel >= lim = Map.empty
    | treeLevel < 3    = nw' `par` ne' `par` sw' `par` (pseq se' result)
    | otherwise        = result
    where
        !result = Map.unions [cnews, nw', ne', sw', se']
        !cnews = Map.fromList [ (I2 xH yH, c)
                              , (I2 xH y1, n)
                              , (I2 x2 yH, e)
                              , (I2 x1 yH, w)
                              , (I2 xH y2, s)
                              ]
        !nw' = quadToMap lim (I2 x1 y1) (I2 xH yH) nw
        !ne' = quadToMap lim (I2 xH y1) (I2 x2 yH) ne
        !sw' = quadToMap lim (I2 x1 yH) (I2 xH y2) sw
        !se' = quadToMap lim (I2 xH yH) (I2 x2 y2) se

        !xH = x1 + (x2-x1) `div` 2
        !yH = y1 + (y2-y1) `div` 2
