module Fdd.Hardware.Domain where

newtype DeviceName = DeviceName String
    deriving stock (Show, Eq, Ord)
newtype ComponentIndex = ComponentIndex String
    deriving stock (Show, Eq, Ord)
newtype ControllerName = ControllerName String
    deriving stock (Show, Eq, Ord)
newtype Controller = Controller ControllerName
    deriving stock (Show, Eq, Ord)

data ControllerStatus
    = ControllerOk
    | ControllerFail String
    deriving stock (Show, Eq, Ord)

data HardwareFailure
    = DeviceNotFound String
    | DevicePartNotFound String
    deriving stock (Show, Eq, Ord)