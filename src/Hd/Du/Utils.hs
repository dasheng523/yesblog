{-# LANGUAGE NamedFieldPuns #-}

module Hd.Du.Utils where

import Hd.Du.App
import System.Directory

traverseDirectoryWith :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith app = do
    curPath <- asks path
    content <- liftIO $ listDirectory curPath
    traverse_ go content
  where
    go name = flip local app $
        \env ->
            env
                { path = path env </> name
                , depth = depth env + 1
                }

traverseDirectoryWith' :: MyApp le s () -> MyApp le s ()
traverseDirectoryWith' app =
    asks path >>= liftIO . listDirectory >>= traverse_ go
  where
    go name = flip local app $
        \env ->
            env
                { path = path env </> name
                , depth = depth env + 1
                }

currentPathStatus :: MyApp l s FileStatus
currentPathStatus = do
    AppEnv{fileStatus, path} <- ask
    liftIO $ fileStatus path

checkExtension :: AppConfig -> FilePath -> Bool
checkExtension cfg fp =
    maybe True (`isExtensionOf` fp) (extension cfg)