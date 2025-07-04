{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Hd.Du.DiskUsage where

import Hd.Du.App
import Hd.Du.Utils

data DUEntryAction
    = TraverseDir {dirpath :: FilePath, requireReporting :: Bool}
    | RecordFileSize {fsize :: FileOffset}
    | None

diskUsage :: MyApp (FilePath, FileOffset) FileOffset ()
diskUsage = liftM2 decide ask currentPathStatus >>= processEntry
  where
    decide AppEnv{..} fs
        | isDirectory fs =
            TraverseDir path (depth <= maxDepth cfg)
        | isRegularFile fs && checkExtension cfg path =
            RecordFileSize (fileSize fs)
        | otherwise = None
    processEntry TraverseDir{..} = do
        usageOnEntry <- get
        when requireReporting $ do
            usageOnExit <- get
            tell [(dirpath, usageOnExit - usageOnEntry)]
    processEntry RecordFileSize{fsize} = modify (+ fsize)
    processEntry None = pass