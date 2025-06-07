module Fdd.Hardware.Impl.Service (
    HardwareService (..),
) where

import Fdd.Hardware.Impl.Device.Types
import Fdd.Hardware.Language.Hdl

data HardwareService = HardwareService
    { makeDevice :: Hdl -> IO Device
    , getBlankDevice :: IO Device
    , getDevicePart :: ComponentIndex -> Device -> IO (Maybe DevicePart)
    }