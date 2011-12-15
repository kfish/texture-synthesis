{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}

module Main (
    main
) where

import Control.Applicative ((<$>))
import Control.Monad (foldM, replicateM_)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Default
import Data.Time.Clock (getCurrentTime)
import Data.ZoomCache
import Data.ZoomCache.Multichannel
import System.Console.GetOpt
import UI.Command

import Graphics.TextureSynthesis

------------------------------------------------------------

data Config = Config
    { noRaw    :: Bool
    , delta    :: Bool
    , zlib     :: Bool
    , variable :: Bool
    , intData  :: Bool
    , label    :: ByteString
    , rate     :: Integer
    , channels :: Int
    , wmLevel  :: Int
    , track    :: TrackNo
    }

instance Default Config where
    def = defConfig

defConfig :: Config
defConfig = Config
    { noRaw    = False
    , delta    = False
    , zlib     = False
    , variable = False
    , intData  = False
    , label    = "texture"
    , rate     = 1000
    , channels = 1
    , wmLevel  = 100
    , track    = 1
    }

data Option = NoRaw
            | Delta
            | ZLib
            | Variable
            | IntData
            | Label String
            | Rate String
            | Channels String
            | Watermark String
            | Track String
    deriving (Eq)

options :: [OptDescr Option]
options = genOptions

genOptions :: [OptDescr Option]
genOptions =
    [ Option ['z'] ["no-raw"] (NoArg NoRaw)
             "Do NOT include raw data in the output"
    , Option ['d'] ["delta"] (NoArg Delta)
             "Delta-encode data"
    , Option ['Z'] ["zlib"] (NoArg ZLib)
             "Zlib-compress data"
    , Option ['b'] ["variable"] (NoArg Variable)
             "Generate variable-rate data"
    , Option ['i'] ["integer"] (NoArg IntData)
             "Generate integer data"
    , Option ['l'] ["label"] (ReqArg Label "label")
             "Set track label"
    , Option ['r'] ["rate"] (ReqArg Rate "data-rate")
             "Set track rate"
    , Option ['c'] ["channels"] (ReqArg Channels "channels")
             "Set number of channels"
    , Option ['w'] ["watermark"] (ReqArg Watermark "watermark")
             "Set high-watermark level"
    , Option ['t'] ["track"] (ReqArg Track "trackNo")
             "Set or select track number"
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
        processOneOption config Delta = do
            return $ config {delta = True}
        processOneOption config ZLib = do
            return $ config {zlib = True}
        processOneOption config Variable = do
            return $ config {variable = True}
        processOneOption config IntData = do
            return $ config {intData = True}
        processOneOption config (Label s) = do
            return $ config {label = C.pack s}
        processOneOption config (Rate s) = do
            return $ config {rate = read s}
        processOneOption config (Channels s) = do
            return $ config {channels = read s}
        processOneOption config (Watermark s) = do
            return $ config {wmLevel = read s}
        processOneOption config (Track s) = do
            return $ config {track = read s}

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
            let spec = oneTrackMultichannel channels (undefined :: Float) delta zlib VariableSR rate' label
            withFileWrite spec (Just now) (not noRaw) (sW >> liftIO mk >>= (\ss -> mapM_ (write track)
                (zip (map SO [10000,10002..]) ss))) path
        else do
            let spec = oneTrackMultichannel channels (undefined :: Float) delta zlib ConstantSR rate' label
            withFileWrite spec (Just now) (not noRaw) (sW >> replicateM_ 100 (liftIO mk >>= mapM_ (write track))) path
    where
        rate' = fromInteger rate
        sW = setWatermark 1 wmLevel

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
            let spec = oneTrackMultichannel channels (undefined :: Double) delta zlib VariableSR rate' label
            withFileWrite spec (Just now) (not noRaw) (sW >> mapM_ (write track)
                (zip (map SO [10000,10002..]) (map (replicate channels) ts))) path
        else do
            let spec = oneTrackMultichannel channels (undefined :: Double) delta zlib ConstantSR rate' label
            withFileWrite spec (Just now) (not noRaw) (sW >> mapM_ (write track) (map (replicate channels) ts)) path
    where
        rate' = fromInteger rate
        sW = setWatermark 1 wmLevel

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
