module Fdd.Hardware.Language.DeviceControl where

import Fdd.Hardware.Common
import Fdd.Hardware.Domain

type DeviceControl next = [DeviceControlMethod next]

data DeviceControlMethod next
    = GetStatus Controller (Either String Status -> next)
    | ReadSensor Controller ComponentIndex (Either String Measurement -> next)