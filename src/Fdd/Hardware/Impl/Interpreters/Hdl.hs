{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Fdd.Hardware.Impl.Interpreters.Hdl where

import qualified Fdd.Hardware.Language.Hdl as L

import qualified Fdd.Hardware.Common as T
import qualified Fdd.Hardware.Domain as T

import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

import Control.Monad.Free (foldFree)
import qualified Data.Map as Map
import Fdd.Hardware.Impl.Runtime

registerDevicePart ::
    RImpl.HardwareRuntime ->
    T.Controller ->
    T.ComponentIndex ->
    T.ComponentPassport ->
    IO ()
registerDevicePart RImpl.HardwareRuntime{_devicesRef, _hardwareServiceRef} controller idx passp = do
    devices <- readIORef _devicesRef
    service <- readIORef _hardwareServiceRef

    case Map.lookup controller devices of
        Nothing -> error $ "Controller not found: " <> show controller
        Just (_, device) -> do
            eDevicePart <- SImpl.makeDevicePart service passp
            case eDevicePart of
                Left err -> error $ toText err
                Right part -> SImpl.addDevicePart service idx part device

registerDevice :: RImpl.HardwareRuntime -> (TImpl.ControllerImpl, TImpl.Device) -> T.Controller -> IO ()
registerDevice RImpl.HardwareRuntime{_devicesRef} deviceImpl ctrl = do
    devices <- readIORef _devicesRef
    writeIORef _devicesRef $ Map.insert ctrl deviceImpl devices

createController :: RImpl.HardwareRuntime -> T.ControllerName -> T.ComponentPassport -> IO TImpl.ControllerImpl
createController RImpl.HardwareRuntime{_hardwareServiceRef} ctrlName passp = do
    service <- readIORef _hardwareServiceRef
    eControllerImpl <- SImpl.makeController service ctrlName passp
    case eControllerImpl of
        Left err -> error $ toText err
        Right controllerImpl -> pure controllerImpl

interpretHdlMethod :: RImpl.HardwareRuntime -> L.HdlMethod a -> IO a
interpretHdlMethod runtime (L.SetupController deviceName ctrlName passp next) = do
    let RImpl.HardwareRuntime{_devicesRef, _hardwareServiceRef} = runtime
    devices <- readIORef _devicesRef
    service <- readIORef _hardwareServiceRef
    eCtrlImpl <- SImpl.makeController service ctrlName passp
    case eCtrlImpl of
        Left err -> error $ toText err
        Right ctrlImpl -> do
            blankDevice <- SImpl.makeBlankDevice service deviceName ctrlImpl
            let ctrl = T.Controller ctrlName
            let devices' = Map.insert ctrl (ctrlImpl, blankDevice) devices
            writeIORef _devicesRef devices'
            pure $ next ctrl
interpretHdlMethod runtime (L.RegisterComponent ctrl idx passp next) = do
    let RImpl.HardwareRuntime{_devicesRef, _hardwareServiceRef} = runtime
    devices <- readIORef _devicesRef
    service <- readIORef _hardwareServiceRef

    let mbDevice = Map.lookup ctrl devices
    case mbDevice of
        Nothing -> error "Device not found"
        Just (ctrlImpl, device) -> do
            eDevicePart <- SImpl.makeDevicePart service passp
            case eDevicePart of
                Left err -> error $ toText err
                Right part -> do
                    SImpl.addDevicePart service idx part device
                    let devices' = Map.insert ctrl (ctrlImpl, device) devices
                    writeIORef _devicesRef devices'
                    pure $ next ()

runHdl :: RImpl.HardwareRuntime -> L.Hdl a -> IO a
runHdl runtime = foldFree (interpretHdlMethod runtime)