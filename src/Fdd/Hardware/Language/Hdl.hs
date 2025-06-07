module Fdd.Hardware.Language.Hdl where

import Fdd.Hardware.Common
import Fdd.Hardware.Domain

type Hdl next = [HdlMethod next]

data HdlMethod next
    = SetupController DeviceName ControllerName ComponentPassport (Controller -> next)
    | RegisterComponent Controller ComponentIndex ComponentPassport