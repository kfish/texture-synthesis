{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Applicative ((<$>))
import Control.Monad.Trans (liftIO)
import Data.Default
import Data.ZoomCache
import Data.ZoomCache.Multichannel
import UI.Command

import Data.ZoomCache.Texture
import Graphics.TextureSynthesis

------------------------------------------------------------

texGen :: Command ()
texGen = defCmd {
          cmdName = "gen"
        , cmdHandler = texGenHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zoom", "foo.zoom")]
        }

texGenHandler :: App () ()
texGenHandler = do
    filenames <- appArgs
    liftIO $ texWriteFile filenames

texWriteFile :: [FilePath] -> IO ()
texWriteFile []       = return ()
texWriteFile (path:_) = do
    ss <- slices <$> flattenTexture 6 <$> genTexture 5
    -- ss <- slices <$> flattenTexture 5 <$> genTexture 4
    -- ss <- slices <$> flattenTexture 3 <$> genTexture 2

    let ts = map TextureSlice ss
        track = oneTrack (undefined :: TextureSlice) False False ConstantSR 1000 "texture"

    withFileWrite track True (mapM_ (write 1) ts) path

------------------------------------------------------------

texGen1d :: Command ()
texGen1d = defCmd {
          cmdName = "gen1d"
        , cmdHandler = texGen1dHandler
        , cmdCategory = "Writing"
        , cmdShortDesc = "Generate 1-dimensional zoom-cache data"
        , cmdExamples = [("Generate a file called foo.zoom", "foo.zoom")]
        }

texGen1dHandler :: App () ()
texGen1dHandler = do
    filenames <- appArgs
    liftIO $ texWriteFile1d filenames

texWriteFile1d :: [FilePath] -> IO ()
texWriteFile1d []       = return ()
texWriteFile1d (path:_) = do
    ss <- slices <$> flattenTexture 9 <$> genTexture 8
    -- ss <- slices <$> flattenTexture 5 <$> genTexture 4
    -- ss <- slices <$> flattenTexture 3 <$> genTexture 2

    let channels = 12

    let ts :: [Double]
        ts = take 100000 $ map (realToFrac . (* 1000.0)) $ concat ss
        -- track = oneTrack (undefined :: Double) False False ConstantSR 1000 "data"
        track = oneTrackMultichannel channels (undefined :: Double) False False VariableSR 1000 "data"

    -- mapM_ print ts

    -- withFileWrite track False (setWatermark 1 100 >> mapM_ (write 1) ts) "texture1D.zoom"
    withFileWrite track False (setWatermark 1 100 >> mapM_ (write 1)
        (zip (map SO [10000,10002..]) (map (replicate channels) ts))) path

------------------------------------------------------------
-- The Application
--

app :: Application () ()
app = def {
          appName = "texture-synthesis"
        , appVersion = "0.1"
        , appAuthors = ["Conrad Parker"]
        , appBugEmail = "conrad@metadecks.org"
        , appShortDesc = "Generate zoom-cache texture files"
        , appLongDesc = longDesc
        , appCategories = ["Reading", "Writing"]
        , appSeeAlso = [""]
        , appProject = "texture-synthesis"
        , appCmds = [ texGen
                    , texGen1d
                    ]
	}

longDesc :: String
longDesc = "Generate zoom-cache files by texture-synthesis"

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain app
