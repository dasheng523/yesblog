{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import BeamExample
import Rel8Example (runApp) -- 导入 Rel8Example 的 runApp
import Server
import Prelude

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  -- runBeamDemo -- Call the Beam demo function
  Rel8Example.runApp -- 调用 Rel8Example 的 runApp
  -- runApp -- 原始的 runApp (Servant 应用)
