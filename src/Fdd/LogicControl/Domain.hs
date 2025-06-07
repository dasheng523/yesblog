module Fdd.LogicControl.Domain where

import Fdd.Common (Key, Message, Value)
import qualified Fdd.Hardware.Language.DeviceControl as L
import qualified Fdd.Hardware.Language.Hdl as L

data LogicControlMethod
    = EvalHdl (L.Hdl LogicControl)
    | EvalDeviceControl (L.DeviceControl LogicControl)
    | Report Message
    | Store Key Value

type LogicControl = [LogicControlMethod]