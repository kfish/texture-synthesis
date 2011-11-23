{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Graphics.TextureSynthesis (
      Texture(..)
    , textureEmpty
    , genTexture
    , genTextureDefault
    , mkTexture
    , flattenTexture
    , slices
) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (runST)
import Control.Parallel
import Data.Function (on)
import Data.List (groupBy)
import Data.Map (Map)
import Data.Ord (comparing)
import qualified Data.Map as Map
import qualified System.Random.MWC as MWC

-- | Point on the 2D plane
data I2 = I2 {
      iX :: !Int
    , iY :: !Int
    }
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

genTexture :: Int -> IO (Texture Float)
genTexture = MWC.withSystemRandom . mkTextureIO

-- | Generate a texture using the default seed
genTextureDefault :: Int -> Texture Float
genTextureDefault lim = runST $ do
    gen <- MWC.create
    mkTexture lim gen

mkTextureIO :: Int -> MWC.GenIO -> IO (Texture Float)
mkTextureIO = mkTexture

mkTexture :: PrimMonad m => Int -> MWC.Gen (PrimState m) -> m (Texture Float)
mkTexture !lim gen = do
    quad <- mkQuad lim 0 0 0 0 0 0.5 0.5 gen
    return (Texture 0 0 0 0 quad)

mkQuad :: (Fractional a, MWC.Variate a, PrimMonad m)
       => Int -> Int -> a -> a -> a -> a
       -> a -> a
       -> MWC.Gen (PrimState m) -> m (QuadTree a)
mkQuad !lim !lvl !tL !tR !bL !bR h range gen
    | lvl >= lim = return QuadNil
    | otherwise  = do
        let !lvl' = lvl+1
            rand = MWC.uniformR (negate range, range) gen
        cR <- rand
        nR <- rand
        eR <- rand
        wR <- rand
        sR <- rand
        let !c' = ((tL + tR + bL + bR) / 4) + cR
            !n' = ((tL + tR) / 2) + nR
            !e' = ((tR + bR) / 2) + eR
            !w' = ((tL + bL) / 2) + wR
            !s' = ((bL + bR) / 2) + sR
        nw' <- mkQuad lim lvl' tL n' w' c' h (range * h) gen
        ne' <- mkQuad lim lvl' n' tR c' e' h (range * h) gen
        sw' <- mkQuad lim lvl' w' c' bL s' h (range * h) gen
        se' <- mkQuad lim lvl' c' e' s' bR h (range * h) gen
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

slices :: [(I2, a)] -> [[a]]
slices = map (map snd) . groupBy ((==) `on` (iX . fst))

flattenTexture :: Int -> Texture a -> [(I2, a)]
flattenTexture !n = Map.assocs . textureToMap n

textureToMap :: Int -> Texture a -> Map I2 a
textureToMap !n Texture{..} = Map.unions
    [ Map.fromList [ (I2 0 0, topLeft)
                   , (I2 l 0, topRight)
                   , (I2 0 l, botLeft)
                   , (I2 l l, botRight)
                   ]
    , quadToMap n (I2 0 0) (I2 l l) tree
    ]
    where
        l = 2^n
   
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
