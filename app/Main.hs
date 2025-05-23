{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import BeamExample
import Server
import Prelude

{- |
 Main entry point.

 `just run` will invoke this function.
-}
main :: IO ()
main = do
  runBeamDemo -- Call the Beam demo function
  runApp
