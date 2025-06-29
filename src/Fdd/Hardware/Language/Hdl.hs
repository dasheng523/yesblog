module Fdd.Hardware.Language.Hdl where

import Control.Monad.Free
import Fdd.Hardware.Common
import Fdd.Hardware.Domain

data HdlMethod next
    = SetupController DeviceName ControllerName ComponentPassport (Controller -> next)
    | RegisterComponent Controller ComponentIndex ComponentPassport (() -> next)

instance Functor HdlMethod where
    fmap f (SetupController deviceName ctrlName passp next) = SetupController deviceName ctrlName passp (f . next)
    fmap f (RegisterComponent controller idx passp next) = RegisterComponent controller idx passp (f . next)

type Hdl a = Free HdlMethod a

setupController :: DeviceName -> ControllerName -> ComponentPassport -> Hdl Controller
setupController deviceName ctrlName passp = liftF $ SetupController deviceName ctrlName passp id

registerComponent :: Controller -> ComponentIndex -> ComponentPassport -> Hdl ()
registerComponent controller idx passp = liftF $ RegisterComponent controller idx passp id