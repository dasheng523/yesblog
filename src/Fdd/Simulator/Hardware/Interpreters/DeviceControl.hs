{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Fdd.Simulator.Hardware.Interpreters.DeviceControl where

import qualified Fdd.Hardware.Language.DeviceControl as L

import qualified Fdd.Hardware.Domain as T

import Fdd.Simulator.Hardware.Device
import Fdd.Simulator.Runtime

import qualified Data.Map as Map

interpretDeviceControlMethod :: SimulatorRuntime -> L.DeviceControlMethod a -> IO a
interpretDeviceControlMethod runtime (L.GetStatus ctrl) = do
    let SimulatorRuntime{simRtControllerSimsVar} = runtime
    ctrlSims <- readMVar simRtControllerSimsVar

    let tryGetStatus = case Map.lookup ctrl ctrlSims of
            Nothing -> pure $ Left $ T.DeviceNotFound $ show ctrl
            Just ControllerSim{ctrlSimRequestVar} -> do
                statusResponseVar <- newEmptyMVar
                putMVar ctrlSimRequestVar $ GetControlerSimStatus statusResponseVar
                ctrlStatus <- takeMVar statusResponseVar
                pure $ Right ctrlStatus
    tryGetStatus
interpretDeviceControlMethod runtime (L.ReadSensor ctrl idx) = do
    let SimulatorRuntime{simRtControllerSimsVar} = runtime
    ctrlSims <- readMVar simRtControllerSimsVar

    let tryReadSensor = case Map.lookup ctrl ctrlSims of
            Nothing -> pure $ Left $ T.DeviceNotFound $ show ctrl
            Just ControllerSim{ctrlSimRequestVar} -> do
                mbMeasurementVar <- newEmptyMVar
                putMVar ctrlSimRequestVar $ ReadSimSensor idx mbMeasurementVar
                mbMeasurement <- takeMVar mbMeasurementVar
                pure $ case mbMeasurement of
                    Nothing -> Left $ T.NoDataFromSensor $ show (ctrl, idx)
                    Just m -> Right m
    tryReadSensor
interpretDeviceControlMethod runtime (L.GetProperty ctrl _ _) = do
    reportError runtime "GetProperty method not implemented in simulator"
    pure $ Left $ T.DeviceNotFound $ show ctrl
interpretDeviceControlMethod runtime (L.EvalCommand ctrl _ _) = do
    reportError runtime "EvalCommand method not implemented in simulator"
    pure $ Left $ T.DeviceNotFound $ show ctrl