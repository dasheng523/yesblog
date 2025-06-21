{-# LANGUAGE GADTs #-}

module Fdd.Hardware.Language.DeviceControl where

import Fdd.Common
import Fdd.Hardware.Common
import Fdd.Hardware.Domain

data DeviceControlMethod a where
    GetStatus :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
    ReadSensor :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure SensorMeasurement)
    GetProperty :: Controller -> PropertyName -> [Param] -> DeviceControlMethod (Either HardwareFailure (Maybe Property))
    EvalCommand :: Controller -> Command -> [Param] -> DeviceControlMethod (Either HardwareFailure CommandResult)

getStatus' :: Controller -> DeviceControlMethod (Either HardwareFailure ControllerStatus)
getStatus' = GetStatus

readSensor' :: Controller -> ComponentIndex -> DeviceControlMethod (Either HardwareFailure SensorMeasurement)
readSensor' = ReadSensor

getProperty' :: Controller -> PropertyName -> [Param] -> DeviceControlMethod (Either HardwareFailure (Maybe Property))
getProperty' = GetProperty

evalCommand' :: Controller -> Command -> [Param] -> DeviceControlMethod (Either HardwareFailure CommandResult)
evalCommand' = EvalCommand