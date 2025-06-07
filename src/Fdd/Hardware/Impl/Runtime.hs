module Fdd.Hardware.Impl.Runtime where

import qualified Fdd.Hardware.Domain as T

import qualified Fdd.Hardware.Impl.Device.Types as TImpl
import qualified Fdd.Hardware.Impl.Service as SImpl

type Devices = Map T.Controller (TImpl.ControllerImpl, TImpl.Device)

data Runtime = Runtime
    { _devices :: Devices
    , _hardwareService :: SImpl.HardwareService
    }