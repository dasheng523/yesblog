module Fdd.Hardware.Impl.Service (
    HardwareService (..),
) where

import Fdd.Hardware.Common
import Fdd.Hardware.Domain
import Fdd.Hardware.Impl.Device.Types

data HardwareService = HardwareService
    { makeController :: ControllerName -> ComponentPassport -> IO (Either String ControllerImpl)
    , makeBlankDevice :: DeviceName -> ControllerImpl -> IO Device
    , makeDevicePart :: ComponentPassport -> IO (Either String DevicePart)
    , addDevicePart :: ComponentIndex -> DevicePart -> Device -> IO ()
    , getDevicePart :: ComponentIndex -> Device -> IO (Maybe DevicePart)
    }