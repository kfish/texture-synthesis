{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Applicative ((<$>))
import Control.Monad (foldM, replicateM_)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as C
import Data.Default
import qualified Data.IntMap as IM
import Data.Time.Clock (getCurrentTime)
import Data.ZoomCache
import Data.ZoomCache.Multichannel()
import System.Console.GetOpt
import UI.Command

import Graphics.TextureSynthesis

------------------------------------------------------------

data Config = Config
    { noRaw    :: Bool
    , channels :: Int
    , wmLevel  :: Int
    , track    :: TrackNo
    , intData  :: Bool
    , variable :: Bool
    , spec     :: TrackSpec
    }

instance Default Config where
    def = defConfig

defConfig :: Config
defConfig = Config
    { noRaw    = False
    , channels = 1
    , wmLevel  = 100
    , track    = 1
    , intData  = False
    , variable = False
    , spec     = def { specDeltaEncode = False
                     , specZlibCompress = False
                     , specName = "texture"
                     }
    }

data Option = NoRaw
            | Channels String
            | Watermark String
            | Track String
            | Delta
            | ZLib
            | Variable
            | IntData
            | Rate String
            | Label String
    deriving (Eq)

options :: [OptDescr Option]
options = genOptions

genOptions :: [OptDescr Option]
genOptions =
    [ Option ['z'] ["no-raw"] (NoArg NoRaw)
             "Do NOT include raw data in the output"
    , Option ['c'] ["channels"] (ReqArg Channels "channels")
             "Set number of channels"
    , Option ['w'] ["watermark"] (ReqArg Watermark "watermark")
             "Set high-watermark level"
    , Option ['t'] ["track"] (ReqArg Track "trackNo")
             "Set or select track number"
    , Option ['d'] ["delta"] (NoArg Delta)
             "Delta-encode data"
    , Option ['Z'] ["zlib"] (NoArg ZLib)
             "Zlib-compress data"
    , Option ['b'] ["variable"] (NoArg Variable)
             "Generate variable-rate data"
    , Option ['i'] ["integer"] (NoArg IntData)
             "Generate integer data"
    , Option ['r'] ["rate"] (ReqArg Rate "data-rate")
             "Set track rate"
    , Option ['l'] ["label"] (ReqArg Label "label")
             "Set track label"
    ]

processArgs :: [String] -> IO (Config, [String])
processArgs args = do
    case getOpt RequireOrder options args of
        (opts, args', [] ) -> do
            config <- processConfig def opts
            return (config, args')
        (_,    _,     _:_) -> return (def, args)

processConfig :: Config -> [Option] -> IO Config
processConfig = foldM processOneOption
    where
        processOneOption config NoRaw = do
            return $ config {noRaw = True}
        processOneOption config (Channels s) = do
            return $ config {channels = read s}
        processOneOption config (Watermark s) = do
            return $ config {wmLevel = read s}
        processOneOption config (Track s) = do
            return $ config {track = read s}

        processOneOption config Delta = do
            return $ config { spec = (spec config){specDeltaEncode = True} }
        processOneOption config ZLib = do
            return $ config { spec = (spec config){specZlibCompress = True} }
        processOneOption config Variable = do
            return $ config { variable = True
                            , spec = (spec config){specSRType = VariableSR}
                            }
        processOneOption config IntData = do
            return $ config { intData = True
                            , spec = setCodec (undefined :: Int) (spec config)
                            }
        processOneOption config (Rate s) = do
            return $ config { spec = (spec config){specRate = fromInteger $ read s} }
        processOneOption config (Label s) = do
            return $ config { spec = (spec config){specName = C.pack s} }

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
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ texWriteFile config filenames

texWriteFile :: Config -> [FilePath] -> IO ()
texWriteFile _          []       = return ()
texWriteFile Config{..} (path:_) = do
    now <- getCurrentTime

    let mk = slices <$> flattenTexture 6 <$> genTexture 5
    -- let mk = slices <$> flattenTexture 5 <$> genTexture 4
    -- let mk = slices <$> flattenTexture 3 <$> genTexture 2

    if variable
        then do
            withFileWrite trackMap (Just now) (not noRaw) (sW >> liftIO mk >>= (\ss -> mapM_ (write track)
                (zip (map SO [10000,10002..]) ss))) path
        else do
            withFileWrite trackMap (Just now) (not noRaw) (sW >> replicateM_ 100 (liftIO mk >>= mapM_ (write track))) path
    where
        trackMap = IM.singleton track spec'
        sW = setWatermark track wmLevel
        spec' | channels == 1 = setCodec (undefined :: Float) spec
              | otherwise     = setCodecMultichannel channels (undefined :: Float) spec

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
    (config, filenames) <- liftIO . processArgs =<< appArgs
    liftIO $ texWriteFile1d config filenames

texWriteFile1d :: Config -> [FilePath] -> IO ()
texWriteFile1d _          []       = return ()
texWriteFile1d Config{..} (path:_) = do
    now <- getCurrentTime

    ss <- slices <$> flattenTexture 9 <$> genTexture 8
    -- ss <- slices <$> flattenTexture 5 <$> genTexture 4
    -- ss <- slices <$> flattenTexture 3 <$> genTexture 2

    let ts :: [Double]
        ts = take 100000 $ map (realToFrac . (* 1000.0)) $ concat ss

    if variable
        then do
            withFileWrite trackMap (Just now) (not noRaw) (sW >> mapM_ (write track)
                (zip (map SO [10000,10002..]) (map (replicate channels) ts))) path
        else do
            withFileWrite trackMap (Just now) (not noRaw) (sW >> mapM_ (write track) (map (replicate channels) ts)) path
    where
        trackMap = IM.singleton track spec'
        sW = setWatermark track wmLevel
        spec' | channels == 1 = setCodec (undefined :: Double) spec
              | otherwise     = setCodecMultichannel channels (undefined :: Double) spec

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
