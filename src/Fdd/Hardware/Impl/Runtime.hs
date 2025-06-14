module Fdd.Hardware.Impl.Runtime where

import qualified Fdd.Hardware.Domain as T

import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

import qualified Data.Map as Map

type DeviceImpl = (TImpl.ControllerImpl, TImpl.Device)
type Devices = Map.Map T.Controller DeviceImpl

data HardwareRuntime = HardwareRuntime
    { _devicesRef :: IORef Devices
    , _hardwareServiceRef :: IORef SImpl.HardwareService
    }

createHardwareRuntime :: SImpl.HardwareService -> IO HardwareRuntime
createHardwareRuntime hService = do
    devicesRef <- newIORef Map.empty
    hServiceRef <- newIORef hService
    pure $ HardwareRuntime devicesRef hServiceRef