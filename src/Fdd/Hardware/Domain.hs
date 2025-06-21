module Fdd.Hardware.Domain where

import Fdd.Common

newtype DeviceName = DeviceName String
    deriving stock (Show, Eq, Ord)
newtype ComponentIndex = ComponentIndex String
    deriving stock (Show, Eq, Ord)
newtype ControllerName = ControllerName String
    deriving stock (Show, Eq, Ord)
newtype Controller = Controller ControllerName
    deriving stock (Show, Eq, Ord)

type Command = String
type ParamName = String

data Param
    = Param String
    | ValueParam Value
    | UnitParam PhysicalUnit Value
    | ComponentIndexParam ComponentIndex
    deriving stock (Show, Eq, Ord)

data CommandResult
    = CommandSuccess [Property]
    | CommandFail String
    deriving stock (Show, Eq, Ord)

data ControllerStatus
    = ControllerOk
    | ControllerFail String
    deriving stock (Show, Eq, Ord)

data HardwareFailure
    = DeviceNotFound String
    | DevicePartNotFound String
    | NoDataFromSensor String
    | DevicePropertyNotSpecified String
    deriving stock (Show, Eq, Ord)