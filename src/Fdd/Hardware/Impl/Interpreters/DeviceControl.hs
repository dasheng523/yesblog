module Fdd.Hardware.Impl.Interpreters.DeviceControl where

import qualified Fdd.Hardware.Common as T
import qualified Fdd.Hardware.Domain as T
import qualified Fdd.Hardware.Impl.Runtime as RImpl
import qualified Fdd.Hardware.Language.DeviceControl as L

interpretDeviceControllMethod ::
    RImpl.Runtime ->
    (RImpl.Runtime -> next -> IO RImpl.Runtime) ->
    L.DeviceControlMethod next ->
    IO RImpl.Runtime
interpretDeviceControllMethod runtime nextInterp (L.GetStatus _ next) = do
    let nextScript = next $ Right T.StatusOk
    nextInterp runtime nextScript
interpretDeviceControllMethod runtime nextInterp (L.ReadSensor _ _ next) = do
    let nextScript = next $ Right $ T.Measurement T.Temperature 100.0
    nextInterp runtime nextScript

runDeviceControl ::
    RImpl.Runtime ->
    (RImpl.Runtime -> next -> IO RImpl.Runtime) ->
    L.DeviceControl next ->
    IO RImpl.Runtime
runDeviceControl runtime _ [] = pure runtime
runDeviceControl runtime nextInterp (m : ms) = do
    runtime' <- interpretDeviceControllMethod runtime nextInterp m
    runDeviceControl runtime' nextInterp ms