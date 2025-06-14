{-# LANGUAGE GADTs #-}

module Fdd.Hardware.Language.DeviceControl where

import Fdd.Hardware.Common
import Fdd.Hardware.Domain

data DeviceControlMethod a where
    GetStatus :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
    ReadSensor :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure Measurement)

getStatus' :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
getStatus' = GetStatus

readSensor' :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure Measurement)
readSensor' = ReadSensor