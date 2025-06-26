{-# LANGUAGE NamedFieldPuns #-}

module Fdd.Simulator.Runtime where

import qualified Fdd.Common as T
import qualified Fdd.Hardware.Domain as T
import qualified Fdd.LogicControl.Domain as T

import Fdd.Simulator.Hardware.Device

import Control.Concurrent (ThreadId)
import qualified Data.Map as Map

data SimulatorRuntime = SimulatorRuntime
    { simRtControllerSimsVar :: MVar (Map.Map T.Controller ControllerSim)
    , simRtMessagesVar :: MVar [String]
    , simRtErrorsVar :: MVar [String]
    , simSimulationsVar :: MVar [ThreadId]
    , simKeyValueDBVar :: MVar (Map.Map T.Key T.Value)
    }

createSimulatorRuntime :: IO SimulatorRuntime
createSimulatorRuntime = do
    simsVar <- newMVar Map.empty
    msgsVar <- newMVar []
    errsVar <- newMVar []
    detachedSimsVar <- newMVar []
    keyValueDBVar <- newMVar Map.empty
    pure $ SimulatorRuntime simsVar msgsVar errsVar detachedSimsVar keyValueDBVar

reportError :: SimulatorRuntime -> String -> IO ()
reportError SimulatorRuntime{simRtErrorsVar} err = do
    errs <- takeMVar simRtErrorsVar
    let errs' = err : errs
    putMVar simRtErrorsVar errs'