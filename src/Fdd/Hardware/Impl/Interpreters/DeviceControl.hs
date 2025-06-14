{-# LANGUAGE NamedFieldPuns #-}

module Fdd.Hardware.Impl.Interpreters.DeviceControl where

import qualified Fdd.Hardware.Language.DeviceControl as L

import qualified Fdd.Hardware.Domain as T

import qualified Fdd.Hardware.Impl.Component as CImpl
import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

import qualified Data.Map as Map
import Fdd.Hardware.Impl.Runtime

getDevice :: IORef RImpl.Devices -> T.Controller -> IO (Maybe (TImpl.ControllerImpl, TImpl.Device))
getDevice devicesRef ctrl = do
    devices <- readIORef devicesRef
    pure $ Map.lookup ctrl devices

interpretDeviceControlMethod :: RImpl.HardwareRuntime -> L.DeviceControlMethod a -> IO a
interpretDeviceControlMethod _ (L.GetStatus _) =
    -- TODO: dummy
    pure $ Right T.ControllerOk
interpretDeviceControlMethod runtime (L.ReadSensor controller idx) = do
    let RImpl.HardwareRuntime{_devicesRef, _hardwareServiceRef} = runtime
    service <- readIORef _hardwareServiceRef
    mbDevice <- getDevice _devicesRef controller
    case mbDevice of
        Nothing -> pure $ Left $ T.DeviceNotFound $ show controller
        Just (_, device) -> do
            mbDevicePart <- SImpl.getDevicePart service idx device
            case mbDevicePart of
                Nothing -> pure $ Left $ T.DevicePartNotFound $ show idx
                Just devicePart -> do
                    measurement <- TImpl.withHandler devicePart $ \handler ->
                        CImpl.readMeasurement handler
                    pure $ Right measurement