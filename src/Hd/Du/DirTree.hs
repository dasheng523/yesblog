{-# LANGUAGE RecordWildCards #-}

module Hd.Du.DirTree where

import Hd.Du.App
import Hd.Du.Utils

dirTree :: MyApp (FilePath, Int) s ()
dirTree = do
    AppEnv{..} <- ask
    fs <- currentPathStatus
    when (isDirectory fs && depth <= maxDepth cfg) $ do
        tell [(takeBaseName path, depth)]
        traverseDirectoryWith dirTree