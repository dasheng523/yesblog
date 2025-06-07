module Fdd.Hardware.Domain where

type DeviceName = String
type ComponentIndex = String
type ControllerName = String

newtype Controller = Controller ControllerName deriving stock (Show, Eq, Ord)

data Status
    = StatusOk
    deriving stock (Show, Eq, Ord)