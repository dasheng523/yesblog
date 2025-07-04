{-# LANGUAGE RecordWildCards #-}

module Hd.Du.FileCounter where

import System.Directory.Extra (listFiles)

import Hd.Du.App
import Hd.Du.Utils

fileCount :: MyApp (FilePath, Int) s ()
fileCount = do
    AppEnv{..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        traverseDirectoryWith fileCount
        files <- liftIO $ listFiles path
        tell [(path, length $ filter (checkExtension cfg) files)]