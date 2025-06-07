module Fdd.Hardware.Impl.Device.Types (
    DeviceName,
    Device (..),
    DevicePart (..),
    WithHandler (..),
    ControllerImpl (..),
) where

import Fdd.Hardware.Domain
import Fdd.Hardware.Impl.Component

data ControllerImpl = ControllerImpl ControllerName VendorComponent

newtype DevicePart = DevicePart VendorComponent
data Device = Device DeviceName ControllerImpl (Map ComponentIndex DevicePart)

class WithHandler handlerAPI where
    withHandler ::
        DevicePart ->
        (handlerAPI -> IO a) ->
        IO a

instance WithHandler SensorAPI where
    withHandler (DevicePart (VendoredSensor _ handler)) f = f handler
    withHandler _ _ = error "Invalid part API handler"

instance WithHandler ControllerAPI where
    withHandler (DevicePart (VendoredController _ handler)) f = f handler
    withHandler _ _ = error "Invalid part API handler"