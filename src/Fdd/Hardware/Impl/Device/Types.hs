module Fdd.Hardware.Impl.Device.Types (
    DeviceName,
    Device (..),
    DevicePart (..),
    WithHandler (..),
) where

import Fdd.Hardware.Impl.Component
import Fdd.Hardware.Language.Hdl

type DeviceName = String

newtype DevicePart = DevicePart VendorComponent
data Device = Device DeviceName (Map ComponentIndex DevicePart)

-- 怎么想到这个class呢？
class WithHandler handlerAPI where
    withHandler ::
        DevicePart ->
        (handlerAPI -> IO ()) ->
        IO ()

instance WithHandler SensorAPI where
    withHandler (DevicePart (VendoredSensor _ handler)) f = f handler
    withHandler _ _ = error "Invalid part API handler"

instance WithHandler ControllerAPI where
    withHandler (DevicePart (VendoredController _ handler)) f = f handler
    withHandler _ _ = error "Invalid part API handler"