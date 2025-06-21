module Fdd.LogicControl.Domain where

import Fdd.Hardware.Domain

type Message = String
type Key = String

data LogicControlFailure
    = HardwareFailure HardwareFailure
    | LogicControlFailure String
    deriving stock (Show, Eq, Ord)